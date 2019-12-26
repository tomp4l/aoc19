module CoordMap = Coord.CoordMap

let ( >> ) = Relude.Function.flipCompose

type sprite =
  | Empty
  | Scaffold
  | UpRobot
  | DownRobot
  | LeftRobot
  | RightRobot
  | FuckedRobot

let spriteToDisplay = function
  | Empty -> "."
  | Scaffold -> "#"
  | UpRobot -> "^"
  | DownRobot -> "v"
  | LeftRobot -> "<"
  | RightRobot -> ">"
  | FuckedRobot -> "X"

module Screen = struct
  type t = sprite CoordMap.t

  let draw = CoordMap.set

  let make = CoordMap.make

  let get c s = CoordMap.getOrElse c Empty s

  let getCoords = CoordMap.keys

  let findUpRobot screen =
    CoordMap.filter (fun _ v -> v = UpRobot) screen
    |> CoordMap.keys |> Relude.List.head |> Relude.Option.getOrThrow

  let output = Coord.output spriteToDisplay Empty
end

module Camera = struct
  exception InvalidCodePoint of int

  let takeImage input =
    let position = ref (0, 0) in
    let screen = ref (Screen.make ()) in
    let nextOutput string =
      let v = int_of_string string in
      let x, y = !position in
      let nextSprite =
        match v with
        | 35 -> Some Scaffold
        | 46 -> Some Empty
        | 60 -> Some LeftRobot
        | 62 -> Some RightRobot
        | 94 -> Some UpRobot
        | 118 -> Some DownRobot
        | 88 -> Some FuckedRobot
        | 10 -> None
        | i -> raise (InvalidCodePoint i)
      in
      match nextSprite with
      | Some s ->
          screen := Screen.draw !position s !screen;
          position := (x + 1, y)
      | None -> position := (0, y + 1)
    in
    Intcode.run ~nextOutput input |> StackSafeFuture.map (fun _ -> !screen)
end

module Robot = struct
  exception NoMoreInput

  let run (compressed, a, b, c) input =
    let input = Relude.List.replaceAt 0 "2" input in
    let fullInput =
      [ compressed; a; b; c; [ Char.code 'n' ]; [] ]
      |> Relude.List.intersperse [ Char.code '\n' ]
      |> Relude.List.flatten
    in
    let remainingInput = ref fullInput in
    let out = ref 0 in
    let nextOutput s = out := int_of_string s in
    let nextInput () =
      match !remainingInput with
      | [] -> raise NoMoreInput
      | h :: tl ->
          remainingInput := tl;
          StackSafeFuture.pure (string_of_int h)
    in
    Intcode.run ~nextOutput ~nextInput input
    |> StackSafeFuture.map (fun _ -> !out)
end

let findIntersection screen =
  let coords = Screen.getCoords screen in
  coords
  |> Relude.List.filter (fun ((x, y) as c) ->
         if Screen.get c screen = Scaffold then
           let u = Screen.get (x, y - 1) screen in
           let d = Screen.get (x, y + 1) screen in
           let l = Screen.get (x - 1, y) screen in
           let r = Screen.get (x + 1, y) screen in
           u = Scaffold && d = Scaffold && l = Scaffold && r = Scaffold
         else false)

let intersectionsToAlignmentParameters =
  Relude.List.map (fun (x, y) -> x * y) >> Relude.List.Int.sum

type direction = Up | Down | Left | Right

type turn = LeftTurn | RightTurn

let nextPosition (x, y) = function
  | Up -> (x, y - 1)
  | Down -> (x, y + 1)
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)

let leftRight = function
  | Up -> (Left, Right)
  | Down -> (Right, Left)
  | Left -> (Down, Up)
  | Right -> (Up, Down)

exception InvalidState

let findPath screen =
  let start = Screen.findUpRobot screen in
  let rec loop pos direction instructions =
    let np = nextPosition pos direction in
    let l, r = leftRight direction in
    let lp = nextPosition pos l in
    let rp = nextPosition pos r in
    if Screen.get np screen = Scaffold then
      match instructions with
      | (p, d) :: tl -> loop np direction ((p, d + 1) :: tl)
      | [] -> raise InvalidState
    else if Screen.get lp screen = Scaffold then
      loop lp l ((LeftTurn, 1) :: instructions)
    else if Screen.get rp screen = Scaffold then
      loop rp r ((RightTurn, 1) :: instructions)
    else Relude.List.reverse instructions
  in
  loop start Up []

let xToChars render path =
  path |> Relude.List.map render
  |> Relude.List.intersperse [ "," ]
  |> Relude.List.flatten
  |> Relude.List.map (fun s -> s.[0])

let pathToChars =
  xToChars (fun (dir, dis) ->
      (match dir with LeftTurn -> "L" | RightTurn -> "R")
      :: ","
      :: (string_of_int dis |> Relude.String.splitList ~delimiter:""))

let charsToAscii = Relude.List.map Char.code

type compressed = Raw of turn * int | Compressed of char

let compressedToChars =
  xToChars (function
    | Raw (dir, dis) ->
        (match dir with LeftTurn -> "L" | RightTurn -> "R")
        :: ","
        :: (string_of_int dis |> Relude.String.splitList ~delimiter:"")
    | Compressed c -> [ Char.escaped c ])

let replace ident sub path =
  let rec loop subRemaining removed remaining replaced =
    match (subRemaining, remaining) with
    | [], _ -> loop sub [] remaining (Compressed ident :: replaced)
    | _, (Compressed _ as n) :: r ->
        let replaced = n :: Relude.List.concat removed replaced in
        loop sub [] r replaced
    | (a, b) :: t1, (Raw (c, d) as r) :: t2 when a = c && b = d ->
        loop t1 (r :: removed) t2 replaced
    | _, h :: t when removed = [] -> loop sub [] t (h :: replaced)
    | _, [] when removed = [] -> Relude.List.reverse replaced
    | _, _ ->
        let replaced = Relude.List.concat removed replaced in
        loop sub [] remaining replaced
  in
  loop sub [] path []

let compress path =
  let pathLength = Relude.List.length path in
  let maxLength = 5 in
  let maxCharLength = 20 in
  let lengths = Relude.List.makeWithIndex maxLength (( + ) 1) in
  let attempts =
    Relude.List.map2
      (fun s e ->
        let start = Relude.List.take s path in
        let end_ = Relude.List.drop (pathLength - e) path in
        (start, end_))
      lengths lengths
    |> Relude.List.filter (fun (s, e) ->
           Relude.List.length (pathToChars s) <= maxCharLength
           && Relude.List.length (pathToChars e) <= maxCharLength)
  in
  let raw = Relude.List.map (fun (d, p) -> Raw (d, p)) path in
  let compressed =
    Relude.List.map
      (fun (s, e) ->
        let a = replace 'A' s raw in
        let b = replace 'B' e a in
        let remaining =
          b
          |> Relude.List.dropWhile (function
               | Compressed _ -> true
               | Raw _ -> false)
          |> Relude.List.takeWhile (function Raw _ -> true | _ -> false)
          |> Relude.List.mapOption (function
               | Raw (a, b) -> Some (a, b)
               | _ -> None)
        in
        let c = replace 'C' remaining b in
        (s, e, remaining, c))
      attempts
  in
  let valid =
    compressed
    |> Relude.List.filter (fun (_, _, _, compressed) ->
           Relude.List.all
             (function Compressed _ -> true | Raw _ -> false)
             compressed)
  in
  let a, b, c, comp = valid |> Relude.List.head |> Relude.Option.getOrThrow in
  ( comp |> compressedToChars |> charsToAscii,
    a |> pathToChars |> charsToAscii,
    b |> pathToChars |> charsToAscii,
    c |> pathToChars |> charsToAscii )

let input = InputLoader.commaSeparated 17

let image = input |> StackSafeFuture.flatMap Camera.takeImage

let _ =
  image
  |> StackSafeFuture.tap
       ( findIntersection >> intersectionsToAlignmentParameters
       >> Js.log2 "sum of the alignment parameters" )

let compressed = image |> StackSafeFuture.map (findPath >> compress)

let _ =
  StackSafeFuture.map2 (fun i c -> Robot.run c i) input compressed
  |> StackSafeFuture.flatten
  |> StackSafeFuture.tap (Js.log2 "Dust")

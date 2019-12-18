module CoordMap = Coord.CoordMap

let ( >> ) = Relude.Function.flipCompose

type sprite = Empty | Wall | Block | HorizontalPaddle | Ball

exception UnknownInput of int

let intToSprite i =
  match i with
  | 0 -> Empty
  | 1 -> Wall
  | 2 -> Block
  | 3 -> HorizontalPaddle
  | 4 -> Ball
  | i -> raise (UnknownInput i)

let spriteToDisplay sprite =
  match sprite with
  | Empty -> " "
  | Wall -> "|"
  | Block -> "#"
  | HorizontalPaddle -> "="
  | Ball -> "*"

module Screen = struct
  type t = sprite CoordMap.t

  let draw = CoordMap.set

  let make = CoordMap.make

  let countBlocks = CoordMap.values >> Relude.List.countBy (( = ) Block)

  exception InvalidScreen

  let ballPaddleXs screen =
    let bp =
      CoordMap.filter (fun _ v -> v = Ball || v = HorizontalPaddle) screen
      |> CoordMap.toList
      |> Relude.List.partition (fun (_, v) -> v = Ball)
    in
    match bp with
    | [ ((xb, _), _) ], [ ((xp, _), _) ] -> (xb, xp)
    | _ -> raise InvalidScreen

  let output = Coord.output spriteToDisplay Empty
end

module Cabinet = struct
  type outputMode = X | Y of int | SpriteOrScore of int * int

  let runGame quarters input =
    let input = string_of_int quarters :: Relude.List.tailOrEmpty input in
    let score = ref 0 in
    let screen = ref (Screen.make ()) in
    let outputMode = ref X in
    let nextOutput string =
      let v = int_of_string string in
      match !outputMode with
      | X -> outputMode := Y v
      | Y x -> outputMode := SpriteOrScore (x, v)
      | SpriteOrScore (-1, 0) ->
          score := v;
          outputMode := X
      | SpriteOrScore (x, y) ->
          let sprite = intToSprite v in
          let screenUpdate = Screen.draw (x, y) sprite !screen in
          screen := screenUpdate;
          outputMode := X
    in
    let nextInput () =
      let xBall, xPaddle = Screen.ballPaddleXs !screen in
      Screen.output !screen;
      Js.log2 "Score: " !score;
      let input =
        match Relude.Int.compare xBall xPaddle with
        | `equal_to -> "0"
        | `greater_than -> "1"
        | `less_than -> "-1"
      in
      StackSafeFuture.pure input
    in

    Intcode.run ~nextOutput ~nextInput input
    |> StackSafeFuture.map (fun _ -> (!screen, !score))
end

let input = InputLoader.commaSeparated 13

let _ =
  input
  |> StackSafeFuture.flatMap (Cabinet.runGame 1)
  |> StackSafeFuture.map (fun (a, _) -> a)
  |> StackSafeFuture.tap (Screen.countBlocks >> Js.log2 "Block count")

let _ =
  input
  |> StackSafeFuture.flatMap (Cabinet.runGame 2)
  |> StackSafeFuture.map (fun (_, a) -> a)
  |> StackSafeFuture.tap (Js.log2 "Score")

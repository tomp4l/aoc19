let ( >> ) = Relude.Function.flipCompose

module Bugs = struct
  type environment = Empty | Bug

  let environmentFromString = function
    | "." -> Some Empty
    | "#" -> Some Bug
    | _ -> None

  let fromString s =
    Relude.String.splitAsList ~delimiter:"\n" s
    |> Relude.List.map
         ( Relude.String.splitAsList ~delimiter:""
         >> Relude.List.mapOption environmentFromString )
    |> Coord.addCoordinates |> Coord.CoordMap.fromList

  let empty = fromString {j|.....
  .....
  .....
  .....
  .....|j}

  let countBugs neighbours =
    Relude.List.countBy (function Bug -> true | Empty -> false) neighbours

  let nextGenerationCell cell neighbours =
    let bugCount = countBugs neighbours in
    match cell with
    | Empty -> if bugCount == 1 || bugCount == 2 then Bug else Empty
    | Bug -> if bugCount == 1 then Bug else Empty

  let nextGeneration env neighbours =
    Coord.CoordMap.mapWithKey
      (fun coord c -> nextGenerationCell c (neighbours coord))
      env

  let mapAllCoords f = Relude.List.map f [ 0; 1; 2; 3; 4 ]

  let neighbours (x, y) l =
    if x == 2 && y == 2 then []
    else
      let left =
        if x == 0 then [ (1, 2, l - 1) ]
        else if x == 3 && y == 2 then mapAllCoords (fun y -> (4, y, l + 1))
        else [ (x - 1, y, l) ]
      in
      let right =
        if x == 4 then [ (3, 2, l - 1) ]
        else if x == 1 && y == 2 then mapAllCoords (fun y -> (0, y, l + 1))
        else [ (x + 1, y, l) ]
      in
      let up =
        if y == 0 then [ (2, 1, l - 1) ]
        else if x == 2 && y == 3 then mapAllCoords (fun x -> (x, 4, l + 1))
        else [ (x, y - 1, l) ]
      in
      let down =
        if y == 4 then [ (2, 3, l - 1) ]
        else if x == 2 && y == 1 then mapAllCoords (fun x -> (x, 0, l + 1))
        else [ (x, y + 1, l) ]
      in
      Relude.List.flatten [ left; right; down; up ]

  let retrieveRecursiveNeighbours neighbours env =
    Relude.List.mapOption
      (fun (x, y, l) ->
        Relude.Int.Map.get l env
        |> Relude.Option.flatMap (fun level -> Coord.CoordMap.get (x, y) level))
      neighbours

  let coordToInt (x, y) =
    let shift = x + (y * 5) in
    1 lsl shift

  let envHealth env =
    Coord.CoordMap.toList env
    |> Relude.List.foldLeft
         (fun h (c, p) -> match p with Bug -> h + coordToInt c | Empty -> h)
         0

  let duplicateHealth env =
    let startHealths = Relude.Int.Set.fromList [ envHealth env ] in
    let rec loop healths lastEnv =
      let nextEnv =
        nextGeneration lastEnv (fun (x, y) ->
            [ (x + 1, y); (x - 1, y); (x, y + 1); (x, y - 1) ]
            |> Relude.List.mapOption (fun c -> Coord.CoordMap.get c lastEnv))
      in
      let health = envHealth nextEnv in
      if Relude.Int.Set.contains health healths then health
      else loop (Relude.Int.Set.update health healths) nextEnv
    in
    loop startHealths env

  let nextGenerationRecursive env =
    let definedLevels = Relude.Int.Map.keys env in
    let min = Relude.List.Int.min definedLevels |> Relude.Option.getOrThrow in
    let max = Relude.List.Int.max definedLevels |> Relude.Option.getOrThrow in
    let rec loopLevels remaining lastEnv =
      match remaining with
      | [] -> lastEnv
      | current :: rest ->
          let level = Relude.Int.Map.getOrElse current empty env in
          let nextLevel =
            nextGeneration level (fun c ->
                let neighbourCoords = neighbours c current in
                retrieveRecursiveNeighbours neighbourCoords env)
          in
          let nextEnv =
            if
              envHealth nextLevel > 0 || Relude.Int.Map.contains current lastEnv
            then Relude.Int.Map.set current nextLevel lastEnv
            else lastEnv
          in
          loopLevels rest nextEnv
    in
    loopLevels ((min - 1) :: (max + 1) :: definedLevels) env

  let countBugsRecursive env =
    let definedLevels = Relude.Int.Map.values env in
    definedLevels
    |> Relude.List.map (fun level -> Coord.CoordMap.values level |> countBugs)
    |> Relude.List.Int.sum

  let drawEnvRecursive gen env =
    Js.log ("Generation", gen);
    Js.log "";
    Relude.Int.Map.toList env
    |> Relude.List.forEach (fun (l, v) ->
           Js.log ("Level", l);
           Coord.output (function Bug -> "#" | Empty -> ".") Empty v;
           Js.log "")

  let show = false

  let runRecusive generations start =
    let startEnv = Relude.Int.Map.fromList [ (0, start) ] in
    let rec loop n lastEnv =
      if show then drawEnvRecursive (generations - n) lastEnv;
      if n == 0 then countBugsRecursive lastEnv
      else
        let nextEnv = nextGenerationRecursive lastEnv in
        loop (n - 1) nextEnv
    in
    loop generations startEnv
end

let input = {j|##.##
.#.##
##..#
#.#..
.###.|j}

let _ = Js.log (Bugs.duplicateHealth (Bugs.fromString input))

let _ = Js.log (Bugs.runRecusive 200 (Bugs.fromString input))

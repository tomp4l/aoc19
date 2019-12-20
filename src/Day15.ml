module CoordMap = Coord.CoordMap

let ( >> ) = Relude.Function.flipCompose

type room = Unknown | Wall | Floor | OxygenSystem

type direction = North | South | East | West

let directionToInt = function North -> 1 | South -> 2 | West -> 3 | East -> 4

let move (x, y) = function
  | North -> (x, y - 1)
  | South -> (x, y + 1)
  | West -> (x - 1, y)
  | East -> (x + 1, y)

let roomToDisplay = function
  | Unknown -> " "
  | Wall -> "#"
  | Floor -> "."
  | OxygenSystem -> "O"

module Ship = struct
  type t = room CoordMap.t

  let get c = CoordMap.get c >> Relude.Option.getOrElse Unknown

  let reveal = CoordMap.set

  let make p r = CoordMap.make () |> reveal p r

  let drawShip = Coord.output roomToDisplay Unknown
end

module Robot = struct
  exception UnknownInput of string

  let nextDirection ship position history =
    let get c = Ship.get c ship in
    let n = move position North |> get in
    let s = move position South |> get in
    let e = move position East |> get in
    let w = move position West |> get in
    match (n, s, e, w) with
    | Unknown, _, _, _ -> (North, North :: history)
    | _, Unknown, _, _ -> (South, South :: history)
    | _, _, Unknown, _ -> (East, East :: history)
    | _, _, _, Unknown -> (West, West :: history)
    | _ ->
        Relude.List.head history
        |> Relude.Option.map (fun d ->
               match d with
               | North -> South
               | South -> North
               | West -> East
               | East -> West)
        |> Relude.Option.map (fun d -> (d, Relude.List.tailOrEmpty history))
        |> Relude.Option.getOrElse (West, [])

  let findOxygenSystem ?(display = false) ?(delay = 0) findSensor input =
    StackSafeFuture.make (fun resolve ->
        let position = ref (0, 0) in
        let lastMove = ref East in
        let ship = ref (Ship.make !position Floor) in
        let history = ref [] in
        let totalDistance = ref 0 in
        let maxDistance = ref 0 in
        let found = ref false in
        let finished = ref false in
        let nextInput () =
          match !finished with
          | true -> StackSafeFuture.never ()
          | _ -> (
              match delay with
              | 0 ->
                  StackSafeFuture.pure
                    (directionToInt !lastMove |> string_of_int)
              | t ->
                  StackSafeFuture.delay t (fun () ->
                      directionToInt !lastMove |> string_of_int) )
        in
        let nextOutput v =
          ( match v with
          | "0" ->
              let attempt = move !position !lastMove in
              ship := Ship.reveal attempt Wall !ship;
              history := Relude.List.tailOrEmpty !history;
              ()
          | "1" ->
              position := move !position !lastMove;
              ( match Ship.get !position !ship with
              | Unknown -> totalDistance := !totalDistance + 1
              | _ -> totalDistance := !totalDistance - 1 );
              ship := Ship.reveal !position Floor !ship;
              ()
          | "2" ->
              position := move !position !lastMove;
              ship := Ship.reveal !position OxygenSystem !ship;
              if findSensor then (
                finished := true;
                resolve (!totalDistance + 1) )
              else if !found then (
                finished := true;
                resolve !maxDistance )
              else (
                ship := Ship.make !position OxygenSystem;
                history := [];
                totalDistance := 0;
                maxDistance := 0;
                found := true );
              ()
          | u -> raise (UnknownInput u) );
          let nextMove, h = nextDirection !ship !position !history in
          lastMove := nextMove;
          history := h;
          maxDistance := Relude.Int.max !maxDistance !totalDistance;
          if display then (
            Js.log "";
            Ship.drawShip !ship;
            Js.log2 "distance" !totalDistance );
          ()
        in
        Intcode.run ~nextInput ~nextOutput input |> ignore)
end

let input = InputLoader.commaSeparated 15

let _ =
  input
  |> StackSafeFuture.flatMap (Robot.findOxygenSystem true)
  |> StackSafeFuture.tap (Js.log2 "Position")

let _ =
  input
  |> StackSafeFuture.flatMap (Robot.findOxygenSystem false)
  |> StackSafeFuture.tap (Js.log2 "Time for oxygen to diffuse")

module CoordMap = Coord.CoordMap

let ( >> ) = Relude.Function.flipCompose

type color = White | Black

type direction = Up | Down | Left | Right

type rotation = Clockwise | Anticlockwise

let rotate d r =
  match (d, r) with
  | Up, Clockwise -> Right
  | Up, Anticlockwise -> Left
  | Down, Clockwise -> Left
  | Down, Anticlockwise -> Right
  | Left, Clockwise -> Up
  | Left, Anticlockwise -> Down
  | Right, Clockwise -> Down
  | Right, Anticlockwise -> Up

let move (x, y) d =
  let x', y' =
    match d with
    | Up -> (0, -1)
    | Down -> (0, 1)
    | Left -> (-1, 0)
    | Right -> (1, 0)
  in
  (x + x', y + y')

let colorToInt c = match c with White -> 1 | Black -> 0

let colorToDisplay c = match c with White -> "#" | Black -> " "

exception UnknownInput of int

let intToColor i =
  match i with 1 -> White | 0 -> Black | i -> raise (UnknownInput i)

let intToRotation i =
  match i with
  | 0 -> Anticlockwise
  | 1 -> Clockwise
  | i -> raise (UnknownInput i)

external write : string -> unit = "write"
  [@@bs.val] [@@bs.scope "process.stdout"]

module Ship = struct
  type t = color CoordMap.t

  let get c = CoordMap.get c >> Relude.Option.getOrElse Black

  let paint = CoordMap.set

  let make c = CoordMap.make () |> paint (0, 0) c

  let totalPainted : t -> int = CoordMap.keys >> Relude.List.length

  let drawShip = Coord.output colorToDisplay Black
end

module Robot = struct
  type t = { position : Coord.coord; direction : direction; isPainting : bool }

  let initial = { position = (0, 0); direction = Up; isPainting = true }

  let doPaint color robot ship =
    let { position;_ } = robot in
    (ship |> Ship.paint position color, { robot with isPainting = false })

  let doRotateAndMove rotation robot =
    let { position; direction;_ } = robot in
    let nextDirection = rotate direction rotation in
    let nextPosition = move position nextDirection in
    { position = nextPosition; direction = nextDirection; isPainting = true }

  let paintShip start input =
    let robot = ref initial in
    let ship = ref (Ship.make start) in
    let nextInput () =
      let { position ;_} = !robot in
      Ship.get position !ship |> colorToInt |> string_of_int
      |> StackSafeFuture.pure
    in
    let nextOutput string =
      let v = int_of_string string in
      let { isPainting ;_} = !robot in
      let nextShip, nextRobot =
        if isPainting then doPaint (intToColor v) !robot !ship
        else (!ship, doRotateAndMove (intToRotation v) !robot)
      in
      ship := nextShip;
      robot := nextRobot;
      ()
    in

    Intcode.run ~nextInput ~nextOutput input
    |> StackSafeFuture.map (fun _ -> !ship)
end

let input = InputLoader.commaSeparated 11

let _ =
  input
  |> StackSafeFuture.flatMap (Robot.paintShip Black)
  |> StackSafeFuture.tap (Ship.totalPainted >> Js.log2 "Total painted")
  |> StackSafeFuture.flatMap (fun _ -> input)
  |> StackSafeFuture.flatMap (Robot.paintShip White)
  |> StackSafeFuture.tap (fun _ -> Js.log "Proper job:")
  |> StackSafeFuture.tap Ship.drawShip

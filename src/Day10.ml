open Coord

let ( >> ) = Relude.Function.flipCompose

type area = Asteroid | EmptySpace | OutOfBounds

module Space = struct
  exception UnknownArea of string

  let fromString s =
    let addCoordinates list =
      let rec loopX l y x =
        match l with
        | v :: rest -> ((x, y), v) :: loopX rest y (x + 1)
        | [] -> []
      in
      let rec loopY l y =
        match l with
        | xs :: rest -> loopX xs y 0 :: loopY rest (y + 1)
        | [] -> []
      in
      Relude.List.flatten (loopY list 0)
    in
    let split =
      s
      |> Relude.String.splitList ~delimiter:"\n"
      |> Relude.List.map
           ( Relude.String.splitList ~delimiter:""
           >> Relude.List.map (fun char ->
                  match char with
                  | "#" -> Asteroid
                  | "." -> EmptySpace
                  | a -> raise (UnknownArea a)) )
      |> addCoordinates
    in
    CoordMap.fromList split

  let size = CoordMap.length

  let get v = CoordMap.get v >> Relude.Option.getOrElse OutOfBounds

  let vaporise v = CoordMap.remove v

  let asteroids space =
    CoordMap.filter (fun _ v -> v = Asteroid) space |> CoordMap.keys
end

let gcm (a, b) =
  if a = 0 then abs b
  else if b = 0 then abs a
  else
    let a = abs a in
    let b = abs b in
    let min = min a b in
    let upTo = Relude.List.makeWithIndex min (( + ) 1) in
    Relude.List.foldLeft
      (fun gcm i -> if a mod i = 0 && b mod i = 0 then i else gcm)
      1 upTo

let isVisible origin space p =
  if p = origin then false
  else
    let vector = sub p origin in
    let gcm = gcm vector in
    let grad = div vector gcm in
    let rec loop p =
      if p = origin then true
      else
        let area = Space.get p space in
        if area = Asteroid then false else loop (sub p grad)
    in
    loop (sub p grad)

let visibleCount space p =
  let asteroids = Space.asteroids space in
  let isVisible = isVisible p space in
  Relude.List.filter isVisible asteroids |> Relude.List.length

let findBestAsteroid space =
  let asteroids = Space.asteroids space in
  asteroids
  |> Relude.List.map (visibleCount space)
  |> Relude.List.zip asteroids
  |> Relude.List.sortBy (fun (_, a) (_, b) -> Relude.Int.compare b a)
  |> Relude.List.map (fun (v, _) -> v)
  |> Relude.List.head |> Relude.Option.getOrThrow

let vectorLength (x, y) =
  let squareSum = (x * x) + (y * y) |> float_of_int in
  Js.Math.sqrt squareSum

let angle (x1, y1) (x2, y2) =
  let determinent = (x1 * y2) - (y1 * x2) |> float_of_int in
  let dotProduct = (x1 * x2) + (y1 * y2) |> float_of_int in
  match Js.Math.atan2 ~y:determinent ~x:dotProduct () with
  | a when a < 0. -> a +. (2. *. Js.Math._PI)
  | a -> a

let minimumAngle origin vector asteroids =
  asteroids
  |> Relude.List.map (fun c -> Coord.sub c origin)
  |> Relude.List.map (angle vector)
  |> Relude.List.zip asteroids
  |> Relude.List.sortBy (fun (_, a) (_, b) -> Relude.Float.compare a b)
  |> Relude.List.map (fun (v, _c) -> v)
  |> Relude.List.head |> Relude.Option.getOrThrow

let findFirstTarget origin space =
  let asteroids = Space.asteroids space in
  let startVector = (0, -1) in
  let visible = asteroids |> Relude.List.filter (isVisible origin space) in
  minimumAngle origin startVector visible

let findNextTarget origin space current =
  let asteroids = Space.asteroids space in
  let vector = Coord.sub current origin in
  let visible =
    asteroids
    |> Relude.List.filter (isVisible origin space)
    |> Relude.List.filter (( = ) current >> not)
  in
  minimumAngle origin vector visible

let bigFuckingLaser origin space desiredCount =
  let firstTarget = findFirstTarget origin space in
  let rec next space current count =
    if count = desiredCount then current
    else
      let nextTarget = findNextTarget origin space current in
      let destroyedTarget = Space.vaporise current space in
      next destroyedTarget nextTarget (count + 1)
  in
  next space firstTarget 1

let _ =
  InputLoader.runIfNotJest (fun () ->
      let input = InputLoader.loadDay 10 in
      input
      |> StackSafeFuture.map Space.fromString
      |> StackSafeFuture.map (fun space -> (space, space |> findBestAsteroid))
      |> StackSafeFuture.tap (fun (space, a) ->
             a |> visibleCount space |> Js.log2 "Best asteroid")
      |> StackSafeFuture.tap (fun (space, a) ->
             let x, y = bigFuckingLaser a space 200 in
             Js.log2 "200th to be boomed" ((x * 100) + y)))

type direction = Right of int | Up of int | Left of int | Down of int

exception InvalidDirection of string

type coord = int * int

let ( >> ) = Relude.Function.Infix.( >> )

let stringToDirection s =
  let split = s |> Relude.String.splitAt 1 in
  match split with
  | "R", i -> Right (int_of_string i)
  | "L", i -> Left (int_of_string i)
  | "U", i -> Up (int_of_string i)
  | "D", i -> Down (int_of_string i)
  | l, _ -> raise (InvalidDirection l)

let distance d = match d with Right i | Left i | Up i | Down i -> i

let pathFromDirection (direction : direction) (coord : coord) : coord list =
  let rec loop path dir coord' =
    let x, y = coord' in
    match distance dir > 0 with
    | true ->
        let nextDistance = distance dir - 1 in
        let nextCoord, nextDirection =
          match dir with
          | Right _ -> ((x + 1, y), Right nextDistance)
          | Left _ -> ((x - 1, y), Left nextDistance)
          | Up _ -> ((x, y + 1), Up nextDistance)
          | Down _ -> ((x, y - 1), Down nextDistance)
        in
        loop (nextCoord :: path) nextDirection nextCoord
    | false -> path
  in
  loop [] direction coord

let pathFromDirections (directions : direction list) : coord list =
  let init = [ (0, 0) ] in
  Relude.List.foldLeft
    (fun acc dir ->
      let current = Relude.List.head acc in
      let path =
        current
        |> Relude.Option.map (pathFromDirection dir)
        |> Relude.Option.toList |> Relude.List.flatten
      in
      Relude.List.concat path acc)
    init directions

module CoordOrd = struct
  type t = coord

  let compare a b =
    let a1, a2 = a in
    let b1, b2 = b in
    match Relude.Int.compare a1 b1 with
    | `equal_to -> Relude.Int.compare a2 b2
    | i -> i

  let eq a b = compare a b == `equal_to
end

module CoordSet = Relude.Set.WithOrd (CoordOrd)
module CoordMap = Relude.Map.WithOrd (CoordOrd)

let getCrossings (a : coord list) (b : coord list) =
  let aSet = CoordSet.fromList a in
  let bSet = CoordSet.fromList b in
  CoordSet.intersect aSet bSet |> CoordSet.remove (0, 0)

let manhattenDistance coord =
  let a, b = coord in
  Relude.Int.abs a + Relude.Int.abs b

let getClosestToOrigin (coords : coord list) =
  Relude.List.sortBy
    (fun a b -> Relude.Int.compare (manhattenDistance a) (manhattenDistance b))
    coords
  |> Relude.List.head |> Relude.Option.getOrThrow

let addDistanceToPath path =
  let pathLength = Relude.List.length path in
  Relude.List.mapWithIndex (fun v i -> (v, pathLength - i - 1)) path

let addDistancesToMap path map =
  Relude.List.foldLeft
    (fun map coordWithDistance ->
      let coord, distance = coordWithDistance in
      match CoordMap.get coord map with
      | Some d -> CoordMap.set coord (d + distance) map
      | None -> map)
    map path

let findClosestIntersection pathA pathB =
  let intersections = getCrossings pathA pathB |> CoordSet.toList in
  let intersectionsWithDistance =
    Belt.List.(zip intersections (make (length intersections) 0))
  in
  let intersectionsMap = CoordMap.fromList intersectionsWithDistance in
  let pathADistances = addDistanceToPath pathA in
  let pathBDistances = addDistanceToPath pathB in
  let intersectionsMap = addDistancesToMap pathADistances intersectionsMap in
  let intersectionsMap = addDistancesToMap pathBDistances intersectionsMap in
  CoordMap.values intersectionsMap |> Relude.List.Int.min

let input =
  InputLoader.newlineSeparated 3
  |. Future.map
       ( Relude.List.map
           ( Relude.String.splitList ~delimiter:","
           >> Relude.List.map stringToDirection )
       >> Relude.List.map pathFromDirections )

let _ =
  input
  |. Future.tap (fun a ->
         match a with
         | [ a; b ] ->
             getCrossings a b |> CoordSet.toList |> getClosestToOrigin
             |> manhattenDistance
             |> Js.log2 "Closest to origin"
         | _ -> ())

let _ =
  input
  |. Future.tap (fun a ->
         match a with
         | [ a; b ] -> findClosestIntersection a b |> Js.log2 "Closest paths"
         | _ -> ())

type orbit = Orbit of string * orbit list

let ( >> ) = Relude.Function.flipCompose

let entryToTuple entry =
  Relude.String.splitList ~delimiter:")" entry
  |> Relude.Tuple.fromList2 |> Relude.Option.getOrThrow

module StringMap = Relude.StringMap
module StringSet = Relude.Set.WithOrd (Relude.String.Ord)

let rec addEntry (p, o) (Orbit (point, orbiters)) =
  match p == point with
  | true -> Orbit (point, Orbit (o, []) :: orbiters)
  | false -> Orbit (point, Relude.List.map (addEntry (p, o)) orbiters)

let entriesToMap entries =
  Relude.List.foldLeft
    (fun map (point, orbiter) ->
      let existing = StringMap.get point map in
      match existing with
      | Some existing -> StringMap.set point (orbiter :: existing) map
      | None -> StringMap.set point [ orbiter ] map)
    (StringMap.make ()) entries

let addEntry = Relude.Function.flip addEntry

let mapToOrbit map =
  let rec loop map orbit =
    let orbittingSet =
      Relude.StringMap.values map |> Relude.List.flatten |> StringSet.fromList
    in
    let orbittedSet = Relude.StringMap.keys map |> StringSet.fromList in
    let planetsNotOrbitting = StringSet.diff orbittedSet orbittingSet in
    let addOrbitAndRemoveFromMap (orbit, map) key =
      let values =
        StringMap.get key map |> Relude.Option.getOrThrow
        |> Relude.List.map (fun v -> (key, v))
      in
      (Relude.List.foldLeft addEntry orbit values, StringMap.remove key map)
    in
    let newOrbit, newMap =
      StringSet.foldLeft addOrbitAndRemoveFromMap (orbit, map)
        planetsNotOrbitting
    in
    match StringMap.isEmpty newMap with
    | true -> newOrbit
    | false -> loop newMap newOrbit
  in
  loop map (Orbit ("COM", []))

let totalOrbitDistance orbit =
  let rec loop count orbit =
    match orbit with
    | Orbit (_, []) -> count
    | Orbit (_, rest) ->
        count + (Relude.List.map (loop (count + 1)) rest |> Relude.List.Int.sum)
  in
  loop 0 orbit

let findPathContaining paths planet =
  Relude.List.find
    (fun a -> Relude.List.contains (module Relude.String.Eq) planet a)
    paths
  |> Relude.Option.getOrElse []

let distanceBetweenYouAndSanta orbit =
  let rec allPaths (Orbit (point, orbiters)) : string list list =
    match orbiters with
    | [] -> [ [ point ] ]
    | _ ->
        Relude.List.flatMap
          (allPaths >> Relude.List.map (fun ps -> point :: ps))
          orbiters
  in
  let allPaths = allPaths orbit in
  let santaPath = findPathContaining allPaths "SAN" |> StringSet.fromList in
  let yourPath = findPathContaining allPaths "YOU" |> StringSet.fromList in
  let differenceSantas = StringSet.diff santaPath yourPath in
  let differenceYours = StringSet.diff yourPath santaPath in
  StringSet.length differenceSantas + StringSet.length differenceYours - 2

let input = InputLoader.newlineSeparated 6

let orbits =
  Future.map input (fun i ->
      Relude.List.map entryToTuple i |> entriesToMap |> mapToOrbit)

let _ =
  Future.tap orbits (fun i ->
      i |> totalOrbitDistance |> Js.log2 "Total path is: ")

let _ =
  Future.tap orbits (fun i ->
      i |> distanceBetweenYouAndSanta |> Js.log2 "Distance: ")

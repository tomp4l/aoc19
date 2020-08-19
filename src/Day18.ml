let ( >> ) = Relude.Function.flipCompose

type pointOfInterest = Start of int | Key of string | Door of string

type cell = Wall | Floor | PointOfInterest of pointOfInterest

module PointOfInterestOrd :
  BsBastet.Interface.ORD with type t = pointOfInterest = struct
  type t = pointOfInterest

  let compare a b =
    match (a, b) with
    | Start i, Start j -> Relude.Int.Ord.compare i j
    | Start _, _ -> `greater_than
    | _, Start _ -> `less_than
    | Key a, Key b -> Relude.String.Ord.compare a b
    | Door a, Door b -> Relude.String.Ord.compare a b
    | Door _, Key _ -> `greater_than
    | Key _, Door _ -> `less_than

  let eq a b = compare a b == `equal_to
end

module PointOfInterestMap = Relude.Map.WithOrd (PointOfInterestOrd)
module PointOfInterestSet = Relude.Set.WithOrd (PointOfInterestOrd)

module KeySet = struct
  type t = int

  let sToT s = 1 lsl ((s.[0] |> Char.code) - Char.code 'a')

  let add s t : t = t lor sToT s

  let empty = 0

  let contains s t = t land sToT s != 0

  let rec length x = if x = 0 then 0 else (1 land x) + length (x lsr 1)

  let fromList xs = Relude.List.foldLeft (fun acc x -> add x acc) empty xs

  let compare a b = Relude.Int.compare a b
end

module PointWithDistance = struct
  type pointWithDistance = { point : pointOfInterest; distance : int }

  module Ord : BsBastet.Interface.ORD with type t = pointWithDistance = struct
    type t = pointWithDistance

    let compare a b =
      let compareDistance = Relude.Int.Ord.compare a.distance b.distance in
      if compareDistance = `equal_to then
        PointOfInterestOrd.compare a.point b.point
      else compareDistance

    let eq a b = compare a b == `equal_to
  end

  module Set = Relude.Set.WithOrd (Ord)
end

module PointWithCollectedKeys = struct
  type pointWithCollectedKeys = {
    point : pointOfInterest;
    collectedKeys : KeySet.t;
  }

  module Ord : BsBastet.Interface.ORD with type t = pointWithCollectedKeys =
  struct
    type t = pointWithCollectedKeys

    let compare a b =
      let compareKey = PointOfInterestOrd.compare a.point b.point in
      if compareKey = `equal_to then
        KeySet.compare a.collectedKeys b.collectedKeys
      else compareKey

    let eq a b = compare a b == `equal_to
  end

  module Map = Relude.Map.WithOrd (Ord)
end

module CollectionState = struct
  type collectionState = {
    points : PointOfInterestSet.t;
    distance : int;
    collectedKeys : KeySet.t;
  }

  let rec comparePointOfIntestSet a b =
    match (PointOfInterestSet.maximum a, PointOfInterestSet.maximum b) with
    | None, None -> `equal_to
    | None, Some _ -> `less_than
    | Some _, None -> `greater_than
    | Some av, Some bv ->
        let c = PointOfInterestOrd.compare av bv in
        if c == `equal_to then
          comparePointOfIntestSet
            (PointOfInterestSet.remove av a)
            (PointOfInterestSet.remove bv b)
        else c

  module Ord : BsBastet.Interface.ORD with type t = collectionState = struct
    type t = collectionState

    let compare a b =
      let compareDistance = Relude.Int.Ord.compare a.distance b.distance in
      if compareDistance = `equal_to then
        let compareKey = comparePointOfIntestSet a.points b.points in
        if compareKey = `equal_to then
          KeySet.compare a.collectedKeys b.collectedKeys
        else compareKey
      else compareDistance

    let eq a b = compare a b == `equal_to
  end

  module Set = Relude.Set.WithOrd (Ord)
  module Map = Relude.Map.WithOrd (Ord)
end

module Maze = struct
  exception UnknownCell of string

  type map = cell Coord.CoordMap.t

  type graph = int PointOfInterestMap.t PointOfInterestMap.t

  let charToCell c =
    match c with
    | "#" -> Wall
    | "." -> Floor
    | "@" -> PointOfInterest (Start 0)
    | uppercase when Relude.String.toUpperCase uppercase = uppercase ->
        PointOfInterest (Door (Relude.String.toLowerCase uppercase))
    | lowercase when Relude.String.toLowerCase lowercase = lowercase ->
        PointOfInterest (Key lowercase)
    | a -> raise (UnknownCell a)

  let mapToGraph (map : map) : graph =
    let emptyGraph = PointOfInterestMap.make () in
    let neighbours (x, y) =
      [ (x, y + 1); (x, y - 1); (x + 1, y); (x - 1, y) ]
    in
    let rec pointsFromPoint' toVisit visited acc =
      match toVisit with
      | [] -> acc
      | (p, _) :: toVisit when Coord.CoordSet.contains p visited ->
          pointsFromPoint' toVisit visited acc
      | (currentPosition, distance) :: stillToVisit -> (
          let newNeighbours =
            neighbours currentPosition
            |> Relude.List.filterNot (fun v ->
                   Coord.CoordSet.contains v visited)
            |> Relude.List.map (fun p -> (p, distance + 1))
          in
          let newVisited = Coord.CoordSet.add currentPosition visited in
          let newToVisit = Relude.List.concat stillToVisit newNeighbours in
          match Coord.CoordMap.get currentPosition map with
          | Some (PointOfInterest _) when distance = 0 ->
              pointsFromPoint' newToVisit newVisited acc
          | Some Floor -> pointsFromPoint' newToVisit newVisited acc
          | Some (PointOfInterest p) ->
              pointsFromPoint' toVisit newVisited
                (PointOfInterestMap.set p distance acc)
          | Some Wall -> pointsFromPoint' toVisit newVisited acc
          | None -> pointsFromPoint' toVisit newVisited acc )
    in
    let pointsFromPoint point =
      let asCell = PointOfInterest point in
      let startCoord, _ =
        Coord.CoordMap.find (fun _ c -> c = asCell) map
        |> Relude.Option.getOrThrow
      in
      pointsFromPoint' [ (startCoord, 0) ] Coord.CoordSet.empty
        (PointOfInterestMap.make ())
    in
    let pointsOfInterest =
      Coord.CoordMap.values map
      |> Relude.List.mapOption (function
           | PointOfInterest a -> Some a
           | _ -> None)
    in
    Relude.List.foldLeft
      (fun g p -> PointOfInterestMap.set p (pointsFromPoint p) g)
      emptyGraph pointsOfInterest

  let fromString string hasFourRobots =
    Relude.String.splitAsList ~delimiter:"\n" string
    |> Relude.List.map
         (Relude.String.splitAsList ~delimiter:"" >> Relude.List.map charToCell)
    |> Coord.addCoordinates |> Coord.CoordMap.fromList
    |> (fun coords ->
         if hasFourRobots then
           let (x, y), _ =
             Coord.CoordMap.find
               (fun _ c -> c = PointOfInterest (Start 0))
               coords
             |> Relude.Option.getOrThrow
           in
           coords
           |> Coord.CoordMap.mergeMany
                (Relude.List.toArray
                   [
                     ((x, y), Wall);
                     ((x + 1, y), Wall);
                     ((x - 1, y), Wall);
                     ((x, y + 1), Wall);
                     ((x, y - 1), Wall);
                     ((x - 1, y - 1), PointOfInterest (Start 1));
                     ((x + 1, y - 1), PointOfInterest (Start 2));
                     ((x - 1, y + 1), PointOfInterest (Start 3));
                     ((x + 1, y + 1), PointOfInterest (Start 4));
                   ])
         else coords)
    |> mapToGraph

  let rec nextKeys' map collectedKeys distances pointsNotVisited keys =
    let filterNotReachedKeysOrLockedDoors =
      PointOfInterestMap.filterNot (fun p _ ->
          match p with
          | Key k -> Relude.StringMap.contains k keys
          | Door d -> not (KeySet.contains d collectedKeys)
          | _ -> false)
    in
    match
      PointWithDistance.Set.filter
        (fun { point; _ } -> PointOfInterestSet.contains point pointsNotVisited)
        distances
      |> PointWithDistance.Set.minimum
    with
    | None -> keys
    | Some closest ->
        let reachable =
          PointOfInterestMap.getOrElse closest.point
            (PointOfInterestMap.make ())
            map
          |> filterNotReachedKeysOrLockedDoors |> PointOfInterestMap.toList
          |> Relude.List.map (fun (p, d) ->
                 PointWithDistance.
                   { point = p; distance = closest.distance + d })
        in
        let keysToCollect, notToCollect =
          Relude.List.partition
            (fun pd ->
              match pd.PointWithDistance.point with
              | Key k -> not (KeySet.contains k collectedKeys)
              | _ -> false)
            reachable
        in
        let closerNotToCollect =
          Relude.List.filter
            (fun PointWithDistance.{ point; distance } ->
              PointWithDistance.Set.filter
                (fun { point = p; _ } -> point = p)
                distances
              |> PointWithDistance.Set.maximum
              |> Relude.Option.map (fun PointWithDistance.{ distance = d; _ } ->
                     d > distance)
              |> Relude.Option.getOrElse true)
            notToCollect
        in
        let nextKeys =
          Relude.List.foldLeft
            (fun acc (k, v) -> Relude.StringMap.set k v acc)
            keys
            (Relude.List.mapOption
               (function
                 | PointWithDistance.{ point = Key k; distance = d } ->
                     Some (k, d)
                 | _ -> None)
               keysToCollect)
        in
        let nextDistances =
          Relude.List.foldLeft
            (fun acc pwd ->
              let removed =
                PointWithDistance.Set.filter
                  (fun { point; _ } -> point = pwd.PointWithDistance.point)
                  acc
                |> PointWithDistance.Set.maximum
                |> Relude.Option.map (fun p ->
                       PointWithDistance.Set.remove p acc)
                |> Relude.Option.getOrElse acc
              in
              PointWithDistance.Set.add pwd removed)
            distances closerNotToCollect
        in
        let nextPointsNotVisited =
          Relude.List.foldLeft
            (fun acc PointWithDistance.{ point; _ } ->
              PointOfInterestSet.add point acc)
            (PointOfInterestSet.remove closest.point pointsNotVisited)
            closerNotToCollect
        in
        nextKeys' map collectedKeys nextDistances nextPointsNotVisited nextKeys

  let nextKeys collectedKeys current map =
    nextKeys' map collectedKeys
      (PointWithDistance.Set.fromList [ { point = current; distance = 0 } ])
      (PointOfInterestSet.fromList [ current ])
      (Relude.StringMap.make ())
    |> Relude.StringMap.toList
    |> Relude.List.sortBy (fun (_, a) (_, b) -> Relude.Int.Ord.compare a b)

  let numberOfKeys map =
    PointOfInterestMap.foldLeft
      (fun c k _ -> match k with Key _ -> c + 1 | _ -> c)
      0 map

  let rec collectKeysFromPoints pointsFrom closest newStates nextKeys =
    match PointOfInterestSet.minimum pointsFrom with
    | None -> newStates
    | Some point ->
        let CollectionState.{ distance; collectedKeys; points } = closest in
        let keys = nextKeys collectedKeys point in
        let newStates' =
          Relude.List.map
            (fun (k, d) ->
              CollectionState.
                {
                  points =
                    points
                    |> PointOfInterestSet.remove point
                    |> PointOfInterestSet.add (Key k);
                  collectedKeys = KeySet.add k collectedKeys;
                  distance = distance + d;
                })
            keys
        in
        collectKeysFromPoints
          (PointOfInterestSet.remove point pointsFrom)
          closest
          (Relude.List.concat newStates newStates')
          nextKeys

  let rec collectAllKeys' targetKeyCount ongoingStates bestDistances nextKeys =
    let closest =
      CollectionState.Set.minimum ongoingStates |> Relude.Option.getOrThrow
    in
    if closest.CollectionState.collectedKeys |> KeySet.length = targetKeyCount
    then closest.CollectionState.distance
    else
      let statesWithoutClosest =
        CollectionState.Set.remove closest ongoingStates
      in
      let CollectionState.{ points; _ } = closest in
      let newStates = collectKeysFromPoints points closest [] nextKeys in
      let closerStates =
        CollectionState.Set.filter
          (fun CollectionState.{ points; collectedKeys; distance } ->
            CollectionState.Map.get
              { points; collectedKeys; distance = 0 }
              bestDistances
            |> Relude.Option.map (fun d -> d > distance)
            |> Relude.Option.getOrElse true)
          (CollectionState.Set.fromList newStates)
      in
      let newBestDistances =
        CollectionState.Set.foldLeft
          (fun acc CollectionState.{ points; collectedKeys; distance } ->
            CollectionState.Map.set
              { points; collectedKeys; distance = 0 }
              distance acc)
          bestDistances closerStates
      in
      let states =
        CollectionState.Set.union statesWithoutClosest closerStates
      in
      collectAllKeys' targetKeyCount states newBestDistances nextKeys

  let nextKeysForMap map =
    let cache = ref (PointWithCollectedKeys.Map.make ()) in
    fun collectedKeys current ->
      let key = PointWithCollectedKeys.{ point = current; collectedKeys } in
      match PointWithCollectedKeys.Map.get key !cache with
      | Some v -> v
      | None ->
          let v = nextKeys collectedKeys current map in
          let _ = cache := PointWithCollectedKeys.Map.set key v !cache in
          v

  let collectAllKeys string =
    let map = fromString string false in
    let targetKeyCount = numberOfKeys map in
    collectAllKeys' targetKeyCount
      (CollectionState.Set.fromList
         [
           {
             points = PointOfInterestSet.fromList [ Start 0 ];
             distance = 0;
             collectedKeys = KeySet.empty;
           };
         ])
      (CollectionState.Map.fromList
         [
           ( {
               points = PointOfInterestSet.fromList [ Start 0 ];
               collectedKeys = KeySet.empty;
               distance = 0;
             },
             0 );
         ])
      (nextKeysForMap map)

  let collectAllKeysFourRobots string =
    let map = fromString string true in
    let targetKeyCount = numberOfKeys map in
    collectAllKeys' targetKeyCount
      (CollectionState.Set.fromList
         [
           {
             points =
               PointOfInterestSet.fromList
                 [ Start 1; Start 2; Start 3; Start 4 ];
             distance = 0;
             collectedKeys = KeySet.empty;
           };
         ])
      (CollectionState.Map.fromList
         [
           ( {
               points =
                 PointOfInterestSet.fromList
                   [ Start 1; Start 2; Start 3; Start 4 ];
               collectedKeys = KeySet.empty;
               distance = 0;
             },
             0 );
         ])
      (nextKeysForMap map)
end

let input = InputLoader.loadDay 18

let _ = input |> StackSafeFuture.tap (Maze.collectAllKeys >> Js.log)

let _ = input |> StackSafeFuture.tap (Maze.collectAllKeysFourRobots >> Js.log)

module SquareWithNeighbours = struct
  type square = Start | End | Floor | InnerPortal | OuterPortal

  type t = { square : square; neighbours : Coord.t list }
end

module StringWithCoords = struct
  type t = string * Coord.t

  module Ord : BsBastet.Interface.ORD with type t = t = struct
    type nonrec t = t

    let compare (s1, c1) (s2, c2) =
      let s = Relude.String.Ord.compare s1 s2 in
      if s == `equal_to then Coord.CoordOrd.compare c1 c2 else s

    let eq a b = compare a b == `equal_to
  end

  module Set = Relude.Set.WithOrd (Ord)
end

module Maze = struct
  let isPortal c = match c with "." | "#" | " " -> false | _ -> true

  let findPortals charMap =
    let rec findPortals' ((x, y) as c) portals =
      match Coord.CoordMap.get c charMap with
      | Some c when isPortal c -> (
          let right = (x + 1, y) in
          let down = (x, y + 1) in
          match
            (Coord.CoordMap.get right charMap, Coord.CoordMap.get down charMap)
          with
          | Some r, _ when isPortal r ->
              let coord =
                match Coord.CoordMap.get (x + 2, y) charMap with
                | Some "." -> (x + 2, y)
                | _ -> (x - 1, y)
              in
              findPortals'
                (x + 1, y)
                (StringWithCoords.Set.add (c ^ r, coord) portals)
          | _, Some d when isPortal d ->
              let coord =
                match Coord.CoordMap.get (x, y + 2) charMap with
                | Some "." -> (x, y + 2)
                | _ -> (x, y - 1)
              in
              findPortals'
                (x + 1, y)
                (StringWithCoords.Set.add (c ^ d, coord) portals)
          | _ -> findPortals' (x + 1, y) portals )
      | Some _ -> findPortals' (x + 1, y) portals
      | None -> if x == 0 then portals else findPortals' (0, y + 1) portals
    in
    findPortals' (0, 0) StringWithCoords.Set.empty

  let createMap width height charMap =
    let portals = findPortals charMap in
    let rec loopMapWithPortals (previousVal, previousCoord) remaining map =
      match StringWithCoords.Set.minimum remaining with
      | Some ((v, ((x, y) as c)) as p) ->
          let outer =
            Relude.List.Int.min [ x; y; width - x; height - y ]
            |> Relude.Option.getOrThrow < 5
          in
          let next =
            if v == previousVal then
              Coord.CoordMap.set previousCoord
                SquareWithNeighbours.
                  {
                    square = (if outer then InnerPortal else OuterPortal);
                    neighbours = [ c ];
                  }
                map
              |> Coord.CoordMap.set c
                   SquareWithNeighbours.
                     {
                       square = (if outer then OuterPortal else InnerPortal);
                       neighbours = [ previousCoord ];
                     }
            else map
          in
          loopMapWithPortals p (StringWithCoords.Set.remove p remaining) next
      | None -> map
    in
    let addCoordinateIfExists (x, y) points =
      match Coord.CoordMap.get (x, y) charMap with
      | Some "." ->
          SquareWithNeighbours.
            { points with neighbours = (x, y) :: points.neighbours }
      | _ -> points
    in
    let rec loopNeighbours ((x, y) as c) map =
      match Coord.CoordMap.get c charMap with
      | None -> if x == 0 then map else loopNeighbours (0, y + 1) map
      | Some "." ->
          let points =
            Coord.CoordMap.get c map
            |> Relude.Option.getOrElse
                 SquareWithNeighbours.{ square = Floor; neighbours = [] }
            |> addCoordinateIfExists (x + 1, y)
            |> addCoordinateIfExists (x - 1, y)
            |> addCoordinateIfExists (x, y + 1)
            |> addCoordinateIfExists (x, y - 1)
          in
          loopNeighbours (x + 1, y) (Coord.CoordMap.set c points map)
      | Some _ -> loopNeighbours (x + 1, y) map
    in
    let startMap =
      match
        ( StringWithCoords.Set.minimum portals,
          StringWithCoords.Set.maximum portals )
      with
      | Some ("AA", min), Some ("ZZ", max) ->
          Coord.CoordMap.make ()
          |> Coord.CoordMap.set min
               SquareWithNeighbours.{ square = Start; neighbours = [] }
          |> Coord.CoordMap.set max
               SquareWithNeighbours.{ square = End; neighbours = [] }
          |> loopMapWithPortals ("AA", min)
               (StringWithCoords.Set.remove ("AA", min) portals)
      | _ -> Coord.CoordMap.make ()
    in
    loopNeighbours (0, 0) startMap

  let fromString s =
    let split =
      s
      |> Relude.String.splitAsList ~delimiter:"\n"
      |> Relude.List.map (Relude.String.splitAsList ~delimiter:"")
    in
    let width =
      Relude.List.head split
      |> Relude.Option.map Relude.List.length
      |> Relude.Option.getOrElse 0
    in
    let height = Relude.List.length split in
    split |> Coord.addCoordinates |> Coord.CoordMap.fromList
    |> createMap width height

  let getStart map =
    match
      Coord.CoordMap.find
        (fun _ SquareWithNeighbours.{ square; _ } ->
          square = SquareWithNeighbours.Start)
        map
      |> Relude.Option.getOrThrow
    with
    | start, _ -> start

  let traverse map =
    let rec traverse visited remaining =
      match remaining with
      | (next, distance) :: others -> (
          match Coord.CoordMap.get next map with
          | _ when Coord.CoordSet.contains next visited ->
              traverse visited others
          | Some SquareWithNeighbours.{ square = End; _ } -> distance
          | Some SquareWithNeighbours.{ neighbours; _ } ->
              traverse
                (Coord.CoordSet.add next visited)
                (Relude.List.concat others
                   (Relude.List.map (fun n -> (n, distance + 1)) neighbours))
          | None -> traverse (Coord.CoordSet.add next visited) others )
      | [] -> -1
    in
    traverse Coord.CoordSet.empty [ (getStart map, 0) ]

  let traverseRecursive map =
    let rec traverse visitedPerLevel remaining =
      match remaining with
      | (next, distance, level) :: others -> (
          let visited =
            Relude.Int.Map.getOrElse level Coord.CoordSet.empty visitedPerLevel
          in
          let addedToVisitedPerLevel =
            Relude.Int.Map.set level
              (Coord.CoordSet.add next visited)
              visitedPerLevel
          in
          match Coord.CoordMap.get next map with
          | _ when Coord.CoordSet.contains next visited ->
              traverse visitedPerLevel others
          | Some SquareWithNeighbours.{ square = End; _ } when level = 1 ->
              distance
          | Some SquareWithNeighbours.{ square = OuterPortal; _ } when level = 1
            ->
              traverse addedToVisitedPerLevel others
          | Some SquareWithNeighbours.{ neighbours; square } ->
              let isHop (x1, y1) (x2, y2) =
                Relude.Int.abs (x1 - x2) + Relude.Int.abs (y1 - y2) > 1
              in
              let newNeighbors =
                Relude.List.map
                  (fun n ->
                    ( n,
                      distance + 1,
                      match square with
                      | OuterPortal -> if isHop next n then level - 1 else level
                      | InnerPortal -> if isHop next n then level + 1 else level
                      | _ -> level ))
                  neighbours
              in
              traverse addedToVisitedPerLevel
                (Relude.List.concat others newNeighbors)
          | None -> traverse addedToVisitedPerLevel others )
      | [] -> -1
    in
    traverse (Relude.Int.Map.make ()) [ (getStart map, 0, 1) ]
end

let input = InputLoader.loadDay 20

let _ =
  input
  |> StackSafeFuture.tap (fun s -> Maze.fromString s |> Maze.traverse |> Js.log)

let _ =
  input
  |> StackSafeFuture.tap (fun s ->
         Maze.fromString s |> Maze.traverseRecursive |> Js.log)

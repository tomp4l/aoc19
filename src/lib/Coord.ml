type coord = int * int

type t = coord

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

let add (a, b) (c, d) = (a + c, b + d)

let sub (a, b) (c, d) = (a - c, b - d)

let div (a, b) c = (a / c, b / c)

external write : string -> unit = "write"
  [@@bs.val] [@@bs.scope "process.stdout"]

let addCoordinates list =
  let rec loopX l y x =
    match l with v :: rest -> ((x, y), v) :: loopX rest y (x + 1) | [] -> []
  in
  let rec loopY l y =
    match l with xs :: rest -> loopX xs y 0 :: loopY rest (y + 1) | [] -> []
  in
  Belt.List.flatten (loopY list 0)

let output toString default map =
  let points = CoordMap.keys map in
  let xs, ys = Relude.List.unzip points in
  let minX = Relude.List.Int.min xs |> Relude.Option.getOrElse 0 in
  let maxX = Relude.List.Int.max xs |> Relude.Option.getOrElse 0 in
  let minY = Relude.List.Int.min ys |> Relude.Option.getOrElse 0 in
  let maxY = Relude.List.Int.max ys |> Relude.Option.getOrElse 0 in
  let rec loopY y =
    if y <= maxY then (
      let rec loopX x =
        if x <= maxX then (
          write
            ( CoordMap.get (x, y) map
            |> Relude.Option.getOrElse default
            |> toString );
          loopX (x + 1) )
      in
      loopX minX;
      write "\n";
      loopY (y + 1) )
  in
  loopY minY

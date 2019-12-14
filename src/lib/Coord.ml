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

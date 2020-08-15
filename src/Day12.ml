type coord3 = int * int * int

let ( >> ) = Relude.Function.flipCompose

module Moon = struct
  type t = { position : coord3; velocity : coord3; id : int }

  let id = ref 0

  let make position =
    id := !id + 1;
    { position; velocity = (0, 0, 0); id = !id }

  let modifyVelocity id (x', y', z') map =
    let module Map = Relude.Int.Map in
    let moon = Map.get id map |> Relude.Option.getOrThrow in
    let { velocity = x, y, z ; _} = moon in
    let moon = { moon with velocity = (x + x', y + y', z + z') } in
    Map.set id moon map

  let makePairs moons =
    let rec pairs moon rest =
      let newPairs = Relude.List.map (fun m -> (moon, m)) rest in
      match rest with m :: r -> newPairs :: pairs m r | _ -> [ newPairs ]
    in
    Relude.List.flatten
      (pairs
         (Relude.List.head moons |> Relude.Option.getOrThrow)
         (Relude.List.tailOrEmpty moons))

  let step moons =
    let module Map = Relude.Int.Map in
    let pairs = makePairs moons in
    let map =
      Map.fromList (moons |> Relude.List.map (fun moon -> (moon.id, moon)))
    in
    pairs
    |> Relude.List.foldLeft
         (fun map (m1, m2) ->
           let { id = id1; position = x1, y1, z1 ;_ } = m1 in
           let { id = id2; position = x2, y2, z2 ;_} = m2 in
           let x' = Relude.Int.Ord.compare x1 x2 |> Relude.Ordering.toInt in
           let y' = Relude.Int.Ord.compare y1 y2 |> Relude.Ordering.toInt in
           let z' = Relude.Int.Ord.compare z1 z2 |> Relude.Ordering.toInt in
           map
           |> modifyVelocity id2 (x', y', z')
           |> modifyVelocity id1 (-x', -y', -z'))
         map
    |> Map.values
    |> Relude.List.map (fun moon ->
           let { position = x, y, z; velocity = x', y', z';_ } = moon in
           { moon with position = (x + x', y + y', z + z') })

  let stepUntil iterations moons =
    let rec loop ms i = if i = iterations then ms else loop (step ms) (i + 1) in
    loop moons 0

  let energy { position = x, y, z; velocity = x', y', z' ;_} =
    (abs x + abs y + abs z) * (abs x' + abs y' + abs z')

  let loopDetector extract moons =
    let extractAll = Relude.List.map extract >> Relude.List.flatten in
    let initial = extractAll moons in
    let rec detect moons i =
      let next = step moons in
      let e = extractAll next in
      match e = initial with true -> i + 1 | false -> detect next (i + 1)
    in
    detect moons 0
end

let lcm nums =
  let bigNums = nums |> Relude.List.map Int64.of_int in
  let rec loop i nums =
    let d, n =
      Relude.List.partition (fun x -> Int64.rem x i = Int64.zero) nums
    in
    match (d, n) with
    | [], [] -> Int64.one
    | [], nums -> loop (Int64.add Int64.one i) nums
    | ds, ns ->
        let divided =
          Relude.List.map (fun d -> Int64.div d i) ds
          |> Relude.List.filterNot (( = ) Int64.one)
        in
        loop i (Relude.List.concat divided ns) |> Int64.mul i
  in

  loop (Int64.of_int 2) bigNums

let initialMoons =
  [
    Moon.make (16, -11, 2);
    Moon.make (0, -4, 7);
    Moon.make (6, 4, -10);
    Moon.make (-3, -2, -4);
  ]

let () =
  Js.log2 " Energy after 1000"
    ( Moon.stepUntil 1000 initialMoons
    |> Relude.List.map Moon.energy
    |> Relude.List.Int.sum )

let xLoop =
  Moon.loopDetector
    (fun { position = x, _, _; velocity = x', _, _ ;_} -> [ x; x' ])
    initialMoons

let yLoop =
  Moon.loopDetector
    (fun { position = _, y, _; velocity = _, y', _ ;_} -> [ y; y' ])
    initialMoons

let zLoop =
  Moon.loopDetector
    (fun { position = _, _, z; velocity = _, _, z';_ } -> [ z; z' ])
    initialMoons

let () =
  Js.log2 "Repeating loop after:"
    (lcm [ xLoop; yLoop; zLoop ] |> Int64.to_string)

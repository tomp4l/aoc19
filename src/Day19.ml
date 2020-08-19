module Tractor = struct
  let runCoordinate intCode x y =
    let sendX = ref true in
    let out = ref 0 in
    let nextInput () =
      ( if !sendX then
        let () = sendX := false in
        x
      else y )
      |> string_of_int |> StackSafeFuture.pure
    in
    let nextOutput s = if s = "1" then out := 1 in
    Intcode.run ~nextOutput ~nextInput intCode
    |> StackSafeFuture.map (fun _ -> !out)

  let scanBeam intCode =
    let rec scanBeam' x y count =
      match (x, y) with
      | 49, 50 -> StackSafeFuture.pure count
      | x, 50 -> scanBeam' (x + 1) 0 count
      | x, y ->
          runCoordinate intCode x y
          |> StackSafeFuture.flatMap (fun v -> scanBeam' x (y + 1) (count + v))
    in
    scanBeam' 0 0 0

  let findArea intCode =
    let rec check next (x, y) r =
      if r = 0 then StackSafeFuture.pure 0
      else
        runCoordinate intCode x y
        |> StackSafeFuture.flatMap (fun i ->
               if i = 0 then StackSafeFuture.pure r
               else check next (next (x, y)) (r - 1))
    in
    let checkX = check (fun (x, y) -> (x + 1, y)) in
    let checkY = check (fun (x, y) -> (x, y + 1)) in
    let lessOne v = Relude.Int.max 1 (v - 1) in
    let rec findArea x y =
      let open StackSafeFuture.Infix in
      runCoordinate intCode x y >>= fun v ->
      if v = 1 then
        checkY (x, y) 100 >>= fun vy ->
        if vy = 0 then
          checkX (x, y) 100 >>= fun vx ->
          if vx = 0 then StackSafeFuture.pure (x, y)
          else findArea x (y + lessOne vx)
        else findArea (x + lessOne vy) 0
      else findArea x (y + 1)
    in
    findArea 100 100
end

let input = InputLoader.commaSeparated 19

let _ =
  input
  |> StackSafeFuture.flatMap Tractor.scanBeam
  |> StackSafeFuture.tap Js.log

let _ =
  input
  |> StackSafeFuture.flatMap Tractor.findArea
  |> StackSafeFuture.tap Js.log

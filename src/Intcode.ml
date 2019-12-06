let position a tape = Relude.Array.at !a tape |> Relude.Option.map ( ! )

let immediate a = Some !a

let getVal mode a tape =
  match mode with
  | "0" -> position a tape
  | "1" -> immediate a
  | m ->
      let _ = Js.Console.error2 "Unknown mode" m in
      None

let op3 tape modes op a b c =
  let at = Relude.Array.at in
  let open Relude.Option in
  let modes = Relude.Tuple.fromListAtLeast2 modes in
  let success =
    flatMap
      (fun (am, bm) ->
        let l = getVal am a tape in
        let r = getVal bm b tape in
        let c = at !c tape in
        let calculated = map2 op l r in
        map2 (fun v state -> v := state) c calculated)
      modes
  in
  isSome success

let args3 pointer tape =
  ( Relude.Array.at (pointer + 1) tape,
    Relude.Array.at (pointer + 2) tape,
    Relude.Array.at (pointer + 3) tape )
  |> Relude.Function.uncurry3 Relude.Option.tuple3

let args2 pointer tape =
  (Relude.Array.at (pointer + 1) tape, Relude.Array.at (pointer + 2) tape)
  |> Relude.Function.uncurry2 Relude.Option.tuple2

let doOp3 tape pointer modes op =
  let args = args3 pointer tape in
  let _ =
    args |> Relude.Option.map (Relude.Function.uncurry3 (op3 tape modes op))
  in
  Some (pointer + 4) |> Future.value

let readInput tape pointer =
  let readline = Readline.make () in
  let open Relude.Option in
  let input = Readline.question readline "input dear human\n" in
  let cell = Relude.Array.at (pointer + 1) tape |> map ( ! ) in
  Future.map input (fun i ->
      cell
      |> flatMap (fun c -> Relude.Array.at c tape)
      |> map (fun v -> v := int_of_string i)
      |> map (Relude.Function.const (pointer + 2)))
  |. Future.tap (fun _ -> Readline.close readline)

let doOutput tape pointer =
  let open Relude.Option in
  let cell = Relude.Array.at (pointer + 1) tape |> map ( ! ) in
  cell
  |> flatMap (fun c -> Relude.Array.at c tape)
  |> map (fun v -> Js.log2 "Output: " !v)
  |> map (Relude.Function.const (pointer + 2))
  |> Future.value

let jumpIf tape pointer modes nonZero =
  let open Relude.Option in
  let modes = Relude.Tuple.fromListAtLeast2 modes in
  let args = args2 pointer tape in
  map2 (fun a b -> (a, b)) args modes
  |> flatMap (fun ((a, b), (am, bm)) ->
         let a' = getVal am a tape in
         let b' = getVal bm b tape in
         map2
           (fun v' p ->
             match (nonZero, v') with
             | false, 0 -> p
             | false, _ | true, 0 -> pointer + 3
             | _ -> p)
           a' b')
  |> Future.value

let run intcode =
  let tape =
    Relude.List.map (fun x -> ref x) intcode |> Relude.Array.fromList
  in
  let rec program pointer =
    let continue =
      match tape |> Relude.Array.at pointer with
      | Some op -> (
          let op = !op |> string_of_int in
          let padded = Relude.String.repeat 5 "0" ^ op in
          let split =
            Relude.String.splitList ~delimiter:"" padded |> Relude.List.reverse
          in
          match split with
          | "1" :: "0" :: modes -> doOp3 tape pointer modes ( + )
          | "2" :: "0" :: modes -> doOp3 tape pointer modes ( * )
          | "3" :: "0" :: _ -> readInput tape pointer
          | "4" :: "0" :: _ -> doOutput tape pointer
          | "5" :: "0" :: modes -> jumpIf tape pointer modes true
          | "6" :: "0" :: modes -> jumpIf tape pointer modes false
          | "7" :: "0" :: modes ->
              doOp3 tape pointer modes (fun a b -> if a < b then 1 else 0)
          | "8" :: "0" :: modes ->
              doOp3 tape pointer modes (fun a b -> if a == b then 1 else 0)
          | "9" :: "9" :: _ -> None |> Future.value
          | _ ->
              let _ = Js.Console.error2 "Unknown op" op in
              None |> Future.value )
      | None ->
          let _ = Js.Console.error2 "Invalid pointer" pointer in
          None |> Future.value
    in
    continue
    |. Future.flatMap (fun continue ->
           match continue with
           | Some pointer -> program pointer
           | None -> Future.value ())
  in
  let done_ = program 0 in
  done_
  |. Future.map (fun () -> tape |> Relude.Array.head |> Relude.Option.map ( ! ))

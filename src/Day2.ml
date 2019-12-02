let doOp fastLookup op a b c =
  let at = Relude.Array.at in
  let open Relude.Option in
  let l = at !a fastLookup |> map ( ! ) in
  let r = at !b fastLookup |> map ( ! ) in
  let c = at !c fastLookup in
  let calculated = map2 op l r in
  let success = map2 (fun v state -> v := state) c calculated in
  isSome success

let run intcode =
  let tape = Relude.List.map (fun x -> ref x) intcode in
  let fastLookup = Relude.Array.fromList tape in
  let doOp = doOp fastLookup in
  let rec program tape =
    let continue =
      match tape with
      | { contents = 1 } :: a :: b :: c :: rest ->
          if doOp ( + ) a b c then Some rest else None
      | { contents = 2 } :: a :: b :: c :: rest ->
          if doOp ( * ) a b c then Some rest else None
      | { contents = 99 } :: _ -> None
      | _ ->
          let _ = Js.Console.error "Unknown op" in
          None
    in
    match continue with Some rest -> program rest | None -> ()
  in
  let _ = program tape in
  tape |> Relude.List.head |> Relude.Option.map ( ! )

let runWithInput i a b =
  let replaced = i |> Relude.List.replaceAt 1 a |> Relude.List.replaceAt 2 b in
  run replaced

let viableInput = Relude.List.makeWithIndex 100 Relude.Function.identity

let findInput input desired =
  let rec search nouns verbs =
    match (nouns, verbs) with
    | noun :: ns, verb :: vs -> (
        match runWithInput input noun verb with
        | Some value when value == desired -> Some ((100 * noun) + verb)
        | _ -> search ns (verb :: vs) )
    | [], _ :: vs -> search viableInput vs
    | _, [] -> None
  in
  search viableInput viableInput

let input = InputLoader.commaSeparatedInts 2

let _ =
  input |. Future.tap (fun i -> runWithInput i 12 2 |> Js.log2 "Processed to:")

let _ =
  input |. Future.tap (fun i -> findInput i 19690720 |> Js.log2 "Desired at:")

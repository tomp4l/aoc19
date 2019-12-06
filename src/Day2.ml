let runWithInput i a b =
  let replaced = i |> Relude.List.replaceAt 1 a |> Relude.List.replaceAt 2 b in
  Intcode.run replaced

let viableInput = Relude.List.makeWithIndex 100 Relude.Function.identity

let findInput input desired =
  let rec search nouns verbs =
    match (nouns, verbs) with
    | noun :: ns, verb :: vs ->
        let matchesDesired a =
          match a with
          | Some v when v == desired ->
              StackSafeFuture.pure ((noun * 100) + verb)
          | _ -> search ns (verb :: vs)
        in
        runWithInput input noun verb |> StackSafeFuture.flatMap matchesDesired
    | [], _ :: vs -> search viableInput vs
    | _, [] -> StackSafeFuture.pure 0
  in
  search viableInput viableInput

let input = InputLoader.commaSeparatedInts 2

let _ =
  input
  |> StackSafeFuture.flatMap (fun i -> runWithInput i 12 2)
  |> StackSafeFuture.tap (Js.log2 "Processed to:")

let _ =
  input
  |> StackSafeFuture.flatMap (fun i -> findInput i 19690720)
  |> StackSafeFuture.tap (Js.log2 "Desired at:")

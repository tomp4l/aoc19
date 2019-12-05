let runWithInput i a b =
  let replaced = i |> Relude.List.replaceAt 1 a |> Relude.List.replaceAt 2 b in
  Intcode.run replaced

let viableInput = Relude.List.makeWithIndex 100 Relude.Function.identity

let findInput input desired =
  let rec search nouns verbs acc =
    match (nouns, verbs) with
    | noun :: ns, verb :: vs ->
        let futureInput =
          runWithInput input noun verb
          |. Future.map2
               (Future.value ((noun * 100) + verb))
               (fun a b -> (a, b))
        in
        search ns (verb :: vs) (futureInput :: acc)
    | [], _ :: vs -> search viableInput vs acc
    | _, [] -> acc
  in
  let allResults = search viableInput viableInput [] in
  Relude.List.foldLeft
    (fun acc f ->
      Future.flatMap acc (fun acc ->
          Future.map f (fun (a, b) ->
              match a with Some v when v == desired -> Some b | _ -> acc)))
    (Future.value None) allResults

let input = InputLoader.commaSeparatedInts 2

let _ =
  input
  |. Future.flatMap (fun i -> runWithInput i 12 2)
  |. Future.tap (Js.log2 "Processed to:")

let _ =
  input
  |. Future.flatMap (fun i -> findInput i 19690720)
  |. Future.tap (Js.log2 "Desired at:")

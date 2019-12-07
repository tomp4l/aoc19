module List = Relude.List
module Option = Relude.Option
module Function = Relude.Function

let combinations a b = List.flatMap (fun a -> List.map (fun b -> (a, b)) b) a

let allInputs viableInputs =
  let allDifferent (a, b, c, d, e) =
    List.Int.sort [ a; b; c; d; e ] = List.Int.sort viableInputs
  in
  viableInputs |> combinations viableInputs |> combinations viableInputs
  |> combinations viableInputs |> combinations viableInputs
  |> List.map (fun (e, (d, (c, (b, a)))) -> (a, b, c, d, e))
  |> List.filter allDifferent

let input = InputLoader.commaSeparatedInts 7

exception NoMoreInput

let programmed input inputs =
  let inputsRef = ref inputs in
  let nextInput () =
    match !inputsRef with
    | next :: remaining ->
        let () = inputsRef := remaining in
        StackSafeFuture.pure next
    | _ ->
        let () = Js.Console.error "No more input" in
        raise NoMoreInput
  in
  StackSafeFuture.make (fun resolve ->
      input |> Intcode.run ~nextInput ~nextOutput:resolve |> ignore)

let runInput computer a b c d e =
  let open StackSafeFuture in
  let a' = computer [ a; 0 ] in
  let b' = a' |> flatMap (fun a' -> computer [ b; a' ]) in
  let c' = b' |> flatMap (fun b' -> computer [ c; b' ]) in
  let d' = c' |> flatMap (fun c' -> computer [ d; c' ]) in
  d' |> flatMap (fun d' -> computer [ e; d' ])

let maxOutput input =
  let runUncurried = Function.uncurry5 (runInput (programmed input)) in
  allInputs [ 0; 1; 2; 3; 4 ]
  |> List.map runUncurried |> StackSafeFuture.all
  |> StackSafeFuture.map List.Int.max

let makeLinkedComputer input const in_ out =
  let useConst = ref true in
  let nextInput () =
    match !useConst with
    | true ->
        let () = useConst := false in
        StackSafeFuture.pure const
    | false -> in_ ()
  in
  Intcode.run input ~nextInput ~nextOutput:out

module Stack = struct
  let pure v = ref [ v ]

  let push a s = s := a :: !s

  let peek s = List.last !s |> Option.getOrThrow

  let pop s =
    let last = peek s in
    let init = List.initOrEmpty !s in
    let () = s := init in
    last
end

type 'a far = FutureAndResponse of 'a StackSafeFuture.t * ('a -> unit)

let futureAndResolve () =
  let res = ref (fun _ -> ()) in
  let fut = StackSafeFuture.make (( := ) res) in
  FutureAndResponse (fut, !res)

let makeIn stack () =
  let (FutureAndResponse (fut, _)) = Stack.peek stack in
  let () = Stack.push (futureAndResolve ()) stack in
  fut

let makeOut stack v =
  let (FutureAndResponse (_, r)) = Stack.pop stack in
  r v

let tieFeedbackLoop lastOutV lastStack =
  let first = ref true in
  let lastOut v =
    let () = lastOutV := v in
    makeOut lastStack v
  in
  let firstIn () =
    let firstIn = makeIn lastStack () in
    let () =
      (* This needs to trigger after the input has been fetched or it will pop too soon *)
      if !first then
        let () = first := false in
        lastOut 0
    in
    firstIn
  in
  (lastOut, firstIn)

let feedbackN input ns =
  let lastOutV = ref 0 in
  let stacks = List.map (fun _ -> Stack.pure (futureAndResolve ())) ns in
  let lastStack = List.last stacks |> Option.getOrThrow in
  let lastOut, firstIn = tieFeedbackLoop lastOutV lastStack in
  let outs =
    List.initOrEmpty stacks |> List.map makeOut |> List.append lastOut
  in
  let ins = firstIn :: (List.initOrEmpty stacks |> List.map makeIn) in
  let computers =
    List.zip ins outs |> List.zip ns
    |> List.map (fun (n, (in_, out)) -> makeLinkedComputer input n in_ out)
  in
  StackSafeFuture.all computers |> StackSafeFuture.map (fun _ -> !lastOutV)

let feedback5 input (a, b, c, d, e) = feedbackN input [ a; b; c; d; e ]

let maxOutputWithFeedback input =
  let runUncurried = feedback5 input in
  allInputs [ 5; 6; 7; 8; 9 ]
  |> List.map runUncurried |> StackSafeFuture.all
  |> StackSafeFuture.map List.Int.max

let _ =
  input
  |> StackSafeFuture.flatMap (fun i -> maxOutput i)
  |> StackSafeFuture.tap (Js.log2 "Max output no feedback")

let _ =
  input
  |> StackSafeFuture.flatMap (fun i -> maxOutputWithFeedback i)
  |> StackSafeFuture.tap (Js.log2 "Max output with feedback")

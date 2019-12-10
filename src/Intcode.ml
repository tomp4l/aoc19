let ( >> ) = Relude.Function.flipCompose

type instructionOutput =
  | Halt
  | Continue
  | AwaitInput of unit StackSafeFuture.t

module Memory = struct
  module Map = Relude.Map.WithOrd (struct
    type t = Int64.t

    let compare a b = Int64.compare a b |> Relude.Ordering.fromInt

    let eq = ( = )
  end)

  type t = string Map.t

  let fromList (list : string list) : t =
    Relude.List.zipWithIndex list
    |> Relude.List.foldLeft
         (fun map (v, i) -> Map.set (Int64.of_int i) v map)
         (Map.make ())

  let get i = Map.get i >> Relude.Option.getOrElse "0"

  let set = Map.set

  let setFromMemory m = Map.set (Int64.of_string m)

  let getFromMemory m =
    Map.get (Int64.of_string m) >> Relude.Option.getOrElse "0"
end

module ComputerState = struct
  type t = {
    mutable memory : Memory.t;
    mutable pointer : Int64.t;
    mutable relativeBase : Int64.t;
  }

  let asNumber = Int64.of_string

  let fromNumber = Int64.to_string

  let fromList (list : string list) =
    {
      memory = Memory.fromList list;
      pointer = Int64.of_int 0;
      relativeBase = Int64.of_int 0;
    }

  let get { memory; pointer } = Memory.get pointer memory

  let increment state =
    state.pointer <- Int64.add state.pointer (Int64.of_int 1)

  let incrementAndGet state =
    let v = get state in
    increment state;
    v

  exception UnknownMode

  let position p { memory } = Memory.getFromMemory p memory

  let relative p { memory; relativeBase } =
    Memory.get (Int64.add (Int64.of_string p) relativeBase) memory

  let incrementAndGetWithMode mode state =
    let v = incrementAndGet state in
    match mode with
    | "0" -> position v state
    | "1" -> v
    | "2" -> relative v state
    | _ -> raise UnknownMode

  let incrementAndSetWithMode mode value state =
    let key = incrementAndGet state |> asNumber in
    let cell =
      match mode with
      | "0" -> key
      | "2" -> Int64.add key state.relativeBase
      | _ -> raise UnknownMode
    in
    state.memory <- Memory.set cell value state.memory

  let doOp3 op (m1, m2, m3) (state : t) =
    let l = (incrementAndGetWithMode m1 >> asNumber) state in
    let r = (incrementAndGetWithMode m2 >> asNumber) state in
    let value = op l r |> fromNumber in
    incrementAndSetWithMode m3 value state;
    Continue

  let add = doOp3 Int64.add

  let mult = doOp3 Int64.mul

  let lt =
    doOp3 (fun a b ->
        if Int64.compare a b < 0 then Int64.of_int 1 else Int64.of_int 0)

  let eq = doOp3 (fun a b -> if a = b then Int64.of_int 1 else Int64.of_int 0)

  let halt _ = Halt

  let input nextInput mode state =
    let input = nextInput () in
    let processed =
      input
      |> StackSafeFuture.map (fun v -> incrementAndSetWithMode mode v state)
    in
    AwaitInput processed

  let output nextOutput mode state =
    let v = (incrementAndGetWithMode mode) state in
    nextOutput v;
    Continue

  let jumpIf (m1, m2) nonZero state =
    let v = (incrementAndGetWithMode m1) state in
    let p = (incrementAndGetWithMode m2 >> asNumber) state in
    ( match (nonZero, v) with
    | false, "0" -> state.pointer <- p
    | true, v when v != "0" -> state.pointer <- p
    | _ -> () );
    Continue

  let adjustBase mode state =
    let v = (incrementAndGetWithMode mode >> asNumber) state in
    state.relativeBase <- Int64.add state.relativeBase v;
    Continue
end

let defaultNextInput () =
  let readline = Readline.make () in
  Readline.question readline "input dear human\n"
  |> StackSafeFuture.tap (fun _ -> Readline.close readline)

let defaultNextOutput : string -> unit = Js.log2 "Output: "

let run ?(nextInput = defaultNextInput) ?(nextOutput = defaultNextOutput)
    (intcode : string list) =
  let state = ComputerState.fromList intcode in
  let rec program () =
    let nextOp =
      let op = ComputerState.incrementAndGet state in
      let padded = Relude.String.repeat 4 "0" ^ op in
      let split =
        Relude.String.splitList ~delimiter:"" padded |> Relude.List.reverse
      in
      match split with
      | "1" :: "0" :: m1 :: m2 :: m3 :: _ -> ComputerState.add (m1, m2, m3)
      | "2" :: "0" :: m1 :: m2 :: m3 :: _ -> ComputerState.mult (m1, m2, m3)
      | "3" :: "0" :: mode :: _ -> ComputerState.input nextInput mode
      | "4" :: "0" :: mode :: _ -> ComputerState.output nextOutput mode
      | "5" :: "0" :: m1 :: m2 :: _ -> ComputerState.jumpIf (m1, m2) true
      | "6" :: "0" :: m1 :: m2 :: _ -> ComputerState.jumpIf (m1, m2) false
      | "7" :: "0" :: m1 :: m2 :: m3 :: _ -> ComputerState.lt (m1, m2, m3)
      | "8" :: "0" :: m1 :: m2 :: m3 :: _ -> ComputerState.eq (m1, m2, m3)
      | "9" :: "0" :: mode :: _ -> ComputerState.adjustBase mode
      | "9" :: "9" :: _ -> ComputerState.halt
      | _ ->
          let _ = Js.Console.error2 "Unknown op" op in
          ComputerState.halt
    in
    let continue = nextOp state in
    match continue with
    | Halt -> StackSafeFuture.pure ()
    | Continue -> program ()
    | AwaitInput future -> future |> StackSafeFuture.flatMap program
  in
  let done_ = program () in
  done_
  |> StackSafeFuture.map (fun () ->
         state.pointer <- Int64.of_int 0;
         ComputerState.get state)

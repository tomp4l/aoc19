let ( >> ) = Relude.Function.flipCompose

type instructionOutput =
  | Halt
  | Continue
  | AwaitInput of unit StackSafeFuture.t

module Memory = struct
  module Map = Relude.Map.WithOrd (Relude.Int.Ord)

  type t = string Map.t

  let fromList (list : string list) : t =
    Relude.List.zipWithIndex list
    |> Relude.List.foldLeft (fun map (v, i) -> Map.set i v map) (Map.make ())

  let get = Map.get

  let set = Map.set

  let setFromMemory m = Map.set (int_of_string m)

  let getFromMemory m = Map.get (int_of_string m)
end

module ComputerState = struct
  type t = {
    mutable memory : Memory.t;
    mutable pointer : int;
    mutable relativeBase : int;
  }

  let asNumber = int_of_string

  let fromNumber = string_of_int

  let fromList (list : string list) =
    { memory = Memory.fromList list; pointer = 0; relativeBase = 0 }

  let get { memory; pointer } =
    Memory.get pointer memory |> Relude.Option.getOrThrow

  let increment state = state.pointer <- state.pointer + 1

  let incrementAndGet state =
    let v = get state in
    increment state;
    v

  exception UnknownMode

  let position p { memory } =
    Memory.getFromMemory p memory |> Relude.Option.getOrThrow

  let incrementAndGetWithMode mode state =
    let v = incrementAndGet state in
    match mode with
    | "0" -> position v state
    | "1" -> v
    | _ -> raise UnknownMode

  let set m v state = state.memory <- Memory.setFromMemory m v state.memory

  let setFromNumber m v = set m (fromNumber v)

  let doOp3 op (m1, m2, _) (state : t) =
    let l = (incrementAndGetWithMode m1 >> asNumber) state in
    let r = (incrementAndGetWithMode m2 >> asNumber) state in
    let location = incrementAndGet state in
    let value = op l r in
    setFromNumber location value state;
    Continue

  let add = doOp3 ( + )

  let mult = doOp3 ( * )

  let lt = doOp3 (fun a b -> if a < b then 1 else 0)

  let eq = doOp3 (fun a b -> if a = b then 1 else 0)

  let halt _ = Halt

  let input nextInput state =
    let input = nextInput () in
    let location = incrementAndGet state in
    let processed =
      input |> StackSafeFuture.map (fun v -> set location v state)
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
      let padded = Relude.String.repeat 5 "0" ^ op in
      let split =
        Relude.String.splitList ~delimiter:"" padded |> Relude.List.reverse
      in
      match split with
      | "1" :: "0" :: m1 :: m2 :: m3 :: _ -> ComputerState.add (m1, m2, m3)
      | "2" :: "0" :: m1 :: m2 :: m3 :: _ -> ComputerState.mult (m1, m2, m3)
      | "3" :: "0" :: _ -> ComputerState.input nextInput
      | "4" :: "0" :: mode :: _ -> ComputerState.output nextOutput mode
      | "5" :: "0" :: m1 :: m2 :: _ -> ComputerState.jumpIf (m1, m2) true
      | "6" :: "0" :: m1 :: m2 :: _ -> ComputerState.jumpIf (m1, m2) false
      | "7" :: "0" :: m1 :: m2 :: m3 :: _ -> ComputerState.lt (m1, m2, m3)
      | "8" :: "0" :: m1 :: m2 :: m3 :: _ -> ComputerState.eq (m1, m2, m3)
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
         state.pointer <- 0;
         ComputerState.get state)

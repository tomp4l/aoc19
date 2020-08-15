type sideEffect = unit -> unit

type executionContext = sideEffect -> unit

type 'a futureState = 'a option ref * sideEffect array ref * executionContext

type 'a future = Future of 'a futureState

type 'a t = 'a future

let mapState (f : 'a -> unit) (state : 'a futureState) : unit =
  match state with
  | { contents = Some v }, _, ec -> ec (fun () -> f v)
  | value, callbacks, _ ->
      callbacks :=
        Relude.Array.append
          (fun () -> match !value with Some v -> f v | None -> ())
          !callbacks

let defaultExecutionContext : executionContext =
  let inFlight = ref [||] in
  let running = ref false in
  let rec runLoop run =
    let () = running := true in
    let () = run () in
    match Relude.Array.head !inFlight with
    | Some runnable ->
        let () = inFlight := Relude.Array.tailOrEmpty !inFlight in
        runLoop runnable
    | None -> running := false
  in
  fun run ->
    match !running with
    | true -> inFlight := Relude.Array.append run !inFlight
    | false -> runLoop run

let make (resolver : ('a -> unit) -> unit) : 'a t =
  let value = ref None in
  let ec = defaultExecutionContext in
  let callbacks = ref [||] in
  let rec loop () =
    match Relude.Array.head !callbacks with
    | Some c ->
        let () = ec c in
        let () = callbacks := Relude.Array.tailOrEmpty !callbacks in
        loop ()
    | None -> ()
  in
  let resolve v =
    let () = value := Some v in
    loop ()
  in
  let () = resolver resolve in
  Future (value, callbacks, ec)

let map : ('a -> 'b) -> 'a t -> 'b t =
 fun f (Future state) ->
  make (fun resolve -> mapState (fun v -> resolve (f v)) state)

let flatMap : ('a -> 'b t) -> 'a t -> 'b t =
 fun f (Future state) ->
  make (fun resolve ->
      mapState
        (fun v ->
          let (Future state) = f v in
          mapState (fun v -> resolve v) state)
        state)

let tap f =
  map (fun v ->
      let () = f v in
      v)

let pure v = make (fun resolve -> resolve v)

let never () = make (fun _ -> ())

external setTimeout : (unit -> unit) -> int -> unit = "setTimeout" [@@bs.val]

let delay t f = make (fun resolve -> setTimeout (fun () -> resolve (f ())) t)

module Functor : BsBastet.Interface.FUNCTOR with type 'a t = 'a t = struct
  type nonrec 'a t = 'a t

  let map = map
end

include Relude_Extensions_Functor.FunctorExtensions (Functor)

let apply f a = flatMap (fun f' -> map (fun a' -> f' a') a) f

module Apply : BsBastet.Interface.APPLY with type 'a t = 'a t = struct
  include Functor

  let apply = apply
end

include Relude_Extensions_Apply.ApplyExtensions (Apply)

module Applicative : BsBastet.Interface.APPLICATIVE with type 'a t = 'a t =
struct
  include Apply

  let pure = pure
end

include Relude_Extensions_Applicative.ApplicativeExtensions (Applicative)

let bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t = fun x f -> flatMap f x

module Monad : BsBastet.Interface.MONAD with type 'a t = 'a t = struct
  include Applicative

  let flat_map = bind
end

include Relude_Extensions_Monad.MonadExtensions (Monad)

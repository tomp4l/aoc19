type _ t =
  | Done : 'a -> 'a t
  | More : (unit -> 'a t) -> 'a t
  | FlatMap : 'b t * ('b -> 'a t) -> 'a t

type ('a, 'b) either = Left of 'a | Right of 'b

let rec flatMap f trampoline =
  match trampoline with
  | FlatMap (sub, cont) -> FlatMap (sub, fun x -> flatMap f (cont x))
  | x -> FlatMap (x, f)

let rec resume trampoline =
  match trampoline with
  | Done v -> Right v
  | More t -> Left t
  | FlatMap (Done v, cont) -> resume (cont v)
  | FlatMap (More t, cont) -> Left (fun () -> FlatMap (t (), cont))
  | FlatMap (FlatMap (sub, cont'), cont) ->
      resume (flatMap (fun x -> flatMap cont (cont' x)) sub)

let rec run trampoline =
  match resume trampoline with Right v -> v | Left m -> run (m ())

let map f = flatMap (fun v -> Done (f v))

module Functor : BsAbstract.Interface.FUNCTOR with type 'a t = 'a t = struct
  type nonrec 'a t = 'a t

  let map = map
end

include Relude_Extensions_Functor.FunctorExtensions (Functor)

let apply f a = flatMap (fun f -> map (fun a -> f a) a) f

module Apply : BsAbstract.Interface.APPLY with type 'a t = 'a t = struct
  include Functor

  let apply = apply
end

include Relude_Extensions_Apply.ApplyExtensions (Apply)

let pure v = Done v

module Applicative : BsAbstract.Interface.APPLICATIVE with type 'a t = 'a t =
struct
  include Apply

  let pure = pure
end

include Relude_Extensions_Applicative.ApplicativeExtensions (Applicative)

let bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t = fun x f -> flatMap f x

module Monad : BsAbstract.Interface.MONAD with type 'a t = 'a t = struct
  include Applicative

  let flat_map = bind
end

include Relude_Extensions_Monad.MonadExtensions (Monad)

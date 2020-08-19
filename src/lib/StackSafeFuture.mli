type 'a t

module Apply : BsBastet.Interface.APPLY with type 'a t = 'a t

module Applicative : BsBastet.Interface.APPLICATIVE with type 'a t = 'a t

module Functor : BsBastet.Interface.FUNCTOR with type 'a t = 'a t

module Monad : BsBastet.Interface.MONAD with type 'a t = 'a t

module Infix : sig
  val ( >>= ) : 'a Monad.t -> ('a -> 'b Monad.t) -> 'b Monad.t

  val ( =<< ) : ('a -> 'b Monad.t) -> 'a Monad.t -> 'b Monad.t

  val ( >=> ) : ('a -> 'b Monad.t) -> ('b -> 'c Monad.t) -> 'a -> 'c Monad.t

  val ( <=< ) : ('a -> 'b Monad.t) -> ('c -> 'a Monad.t) -> 'c -> 'b Monad.t
end

val make : (('a -> unit) -> unit) -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val tap : ('a -> unit) -> 'a t -> 'a t

val pure : 'a -> 'a t

val never : unit -> 'a t

val delay : int -> (unit -> 'a) -> 'a t

val flipMap : 'a Functor.t -> ('a -> 'b) -> 'b Functor.t

val void : 'a Functor.t -> unit Functor.t

val voidRight : 'a -> 'b Functor.t -> 'a Functor.t

val voidLeft : 'a Functor.t -> 'b -> 'b Functor.t

val flap : ('a -> 'b) Functor.t -> 'a -> 'b Functor.t

val apply : ('a -> 'b) t -> 'a t -> 'b t

val applyFirst : 'a Apply.t -> 'b Apply.t -> 'a Apply.t

val applySecond : 'a Apply.t -> 'b Apply.t -> 'b Apply.t

val map2 : ('a -> 'b -> 'c) -> 'a Apply.t -> 'b Apply.t -> 'c Apply.t

val map3 :
  ('a -> 'b -> 'c -> 'd) -> 'a Apply.t -> 'b Apply.t -> 'c Apply.t -> 'd Apply.t

val map4 :
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  'a Apply.t ->
  'b Apply.t ->
  'c Apply.t ->
  'd Apply.t ->
  'e Apply.t

val map5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a Apply.t ->
  'b Apply.t ->
  'c Apply.t ->
  'd Apply.t ->
  'e Apply.t ->
  'f Apply.t

val tuple2 : 'a Apply.t -> 'b Apply.t -> ('a * 'b) Apply.t

val tuple3 : 'a Apply.t -> 'b Apply.t -> 'c Apply.t -> ('a * 'b * 'c) Apply.t

val tuple4 :
  'a Apply.t ->
  'b Apply.t ->
  'c Apply.t ->
  'd Apply.t ->
  ('a * 'b * 'c * 'd) Apply.t

val tuple5 :
  'a Apply.t ->
  'b Apply.t ->
  'c Apply.t ->
  'd Apply.t ->
  'e Apply.t ->
  ('a * 'b * 'c * 'd * 'e) Apply.t

val mapTuple2 : ('a -> 'b -> 'c) -> 'a Apply.t * 'b Apply.t -> 'c Apply.t

val mapTuple3 :
  ('a -> 'b -> 'c -> 'd) -> 'a Apply.t * 'b Apply.t * 'c Apply.t -> 'd Apply.t

val mapTuple4 :
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  'a Apply.t * 'b Apply.t * 'c Apply.t * 'd Apply.t ->
  'e Apply.t

val mapTuple5 :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  'a Apply.t * 'b Apply.t * 'c Apply.t * 'd Apply.t * 'e Apply.t ->
  'f Apply.t

val liftA1 : ('a -> 'b) -> 'a Applicative.t -> 'b Applicative.t

val all : 'a Applicative.t list -> 'a list Applicative.t

val bind : 'a t -> ('a -> 'b t) -> 'b t

val flatMap : ('a -> 'b Monad.t) -> 'a Monad.t -> 'b Monad.t

val flatten : 'a Monad.t Monad.t -> 'a Monad.t

val composeKleisli :
  ('a -> 'b Monad.t) -> ('b -> 'c Monad.t) -> 'a -> 'c Monad.t

val flipComposeKleisli :
  ('b -> 'c Monad.t) -> ('a -> 'b Monad.t) -> 'a -> 'c Monad.t

val liftM1 : ('a -> 'b) -> 'a Monad.t -> 'b Monad.t

val when_ : bool Monad.t -> unit Monad.t -> unit Monad.t

val unless : bool Monad.t -> unit Monad.t -> unit Monad.t

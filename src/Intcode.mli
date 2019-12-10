val run :
  ?nextInput:(unit -> string Aoc19.StackSafeFuture.t) ->
  ?nextOutput:(string -> unit) ->
  string list ->
  string Aoc19.StackSafeFuture.t

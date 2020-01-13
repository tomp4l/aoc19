val run :
  ?nextInput:(unit -> string StackSafeFuture.t) ->
  ?nextOutput:(string -> unit) ->
  string list ->
  string StackSafeFuture.t

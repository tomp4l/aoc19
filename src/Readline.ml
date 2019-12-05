type in_

type out

type t

type inOut = { input : in_; output : out } [@@bs.deriving abstract]

external stdin : in_ = "stdin" [@@bs.val] [@@bs.scope "process"]

external stdout : out = "stdout" [@@bs.val] [@@bs.scope "process"]

external createInterface : inOut -> t = "createInterface"
  [@@bs.module "readline"]

external question : t -> string -> (string -> unit) -> unit = "question"
  [@@bs.send]

external close : t -> unit = "close" [@@bs.send]

let defaultInput = inOut ~input:stdin ~output:stdout

let make () = createInterface defaultInput

let question readline query = Future.make (readline |. question query)

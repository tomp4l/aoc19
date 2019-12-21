external __dirname : string = "__dirname" [@@bs.val]

external env : string Js.Dict.t = "env" [@@bs.scope "process"] [@@bs.val]

let isJest = Js.Dict.get env "JEST_WORKER_ID" |> Relude.Option.isSome

let resolveIfNotJest p = if isJest then StackSafeFuture.never () else p

external readFileWithEncoding :
  string -> string -> (Js.Exn.t Js.Nullable.t -> string -> unit) -> unit
  = "readFile"
  [@@bs.val] [@@bs.module "fs"]

let loadDay number =
  let filename = __dirname ^ "/../input/day" ^ string_of_int number ^ ".txt" in
  StackSafeFuture.make (fun resolve ->
      readFileWithEncoding filename "utf-8" (fun _ data -> resolve data))
  |> resolveIfNotJest

let separated delimiter number =
  loadDay number |> StackSafeFuture.map (Relude.String.splitList ~delimiter)

let charList = separated ""

let newlineSeparated = separated "\n"

let commaSeparated = separated ","

let mapToInts = StackSafeFuture.map (Relude.List.map int_of_string)

let intList =
  let open Relude.Function.Infix in
  charList >> mapToInts

let newlineSeparatedInts =
  let open Relude.Function.Infix in
  newlineSeparated >> mapToInts

let commaSeparatedInts =
  let open Relude.Function.Infix in
  commaSeparated >> mapToInts

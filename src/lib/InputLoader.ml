external __dirname : string = "__dirname" [@@bs.val]

external readFileWithEncoding :
  string -> string -> (Js.Exn.t Js.Nullable.t -> string -> unit) -> unit
  = "readFile"
  [@@bs.val] [@@bs.module "fs"]

let loadDay number =
  let filename = __dirname ^ "/../input/day" ^ string_of_int number ^ ".txt" in
  Future.make (fun resolve ->
      readFileWithEncoding filename "utf-8" (fun _ data -> resolve data))

let separated delimiter number =
  loadDay number |. Future.map (Relude.String.splitList ~delimiter)

let newlineSeparated = separated "\n"

let commaSeparated = separated ","

let mapToInts = Relude.Function.flip Future.map (Relude.List.map int_of_string)

let newlineSeparatedInts =
  let open Relude.Function.Infix in
  newlineSeparated >> mapToInts

let commaSeparatedInts =
  let open Relude.Function.Infix in
  commaSeparated >> mapToInts

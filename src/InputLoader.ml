external __dirname : string = "__dirname" [@@bs.val]

external readFileWithEncoding :
  string -> string -> (Js.Exn.t Js.Nullable.t -> string -> unit) -> unit
  = "readFile"
  [@@bs.val] [@@bs.module "fs"]

let loadDay number =
  let filename = __dirname ^ "/input/day" ^ string_of_int number ^ ".txt" in
  Future.make (fun resolve ->
      readFileWithEncoding filename "utf-8" (fun _ data -> resolve data))

let loadDayAsList number =
  loadDay number |. Future.map (Relude.String.splitList ~delimiter:"\n")

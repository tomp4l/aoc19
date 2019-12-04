let min = 356261

let max = 846303

let ( >> ) = Relude.Function.Infix.( >> )

let containsDoubleDigit string =
  [ "00"; "11"; "22"; "33"; "44"; "55"; "66"; "77"; "88"; "99" ]
  |> Relude.List.any (fun search -> Relude.String.contains ~search string)

let containsDoubleDigitAlone string =
  let digits = Relude.String.splitList ~delimiter:"" string in
  match digits with
  | [ a; b; c; _; _; _ ] when a == b && b != c -> true
  | [ a; b; c; d; _; _ ] when a != b && b == c && c != d -> true
  | [ _; a; b; c; d; _ ] when a != b && b == c && c != d -> true
  | [ _; _; a; b; c; d ] when a != b && b == c && c != d -> true
  | [ _; _; _; a; b; c ] when a != b && b == c -> true
  | _ -> false

let increasingDigits string =
  let digits =
    Relude.String.splitList ~delimiter:"" string
    |> Relude.List.map int_of_string
  in
  Relude.List.zip
    (Relude.List.initOrEmpty digits)
    (Relude.List.tailOrEmpty digits)
  |> Relude.List.all (fun (a, b) -> a <= b)

let validPassword string = containsDoubleDigit string && increasingDigits string

let validPasswordNew string =
  containsDoubleDigitAlone string && increasingDigits string

let range =
  Relude.List.makeWithIndex (max - min + 1) (fun i -> string_of_int (i + min))

let _ =
  Relude.List.filter validPassword range
  |> Relude.List.length
  |> Js.log2 "Potential passwords amount: "

let _ =
  Relude.List.filter validPasswordNew range
  |> Relude.List.length
  |> Js.log2 "New potential passwords amount: "

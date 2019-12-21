let mask size pass =
  let unitSize = 4 * pass in
  let wholeMask =
    Relude.List.makeWithIndex (size + 1) (fun i ->
        let x = i / unitSize in
        let y = i - (x * unitSize) in
        match y with
        | i when i < pass -> 0
        | i when i < 2 * pass -> 1
        | i when i < 3 * pass -> 0
        | _ -> -1)
  in
  Relude.List.tailOrEmpty wholeMask

let lastDigit d =
  d |> string_of_int |> Relude.String.sliceToEnd (-1) |> int_of_string

let pass size pass number =
  let mask = mask size pass in
  Relude.List.zip mask number
  |> Relude.List.map (fun (a, b) -> a * b)
  |> Relude.List.Int.sum |> lastDigit

let phase size number =
  let rec loop p acc =
    if p = 0 then acc
    else
      let pass = pass size p number in
      loop (p - 1) (pass :: acc)
  in
  loop size []

let phases amount size number =
  let rec loop amount number =
    if amount > 0 then loop (amount - 1) (phase size number) else number
  in
  loop amount number

let repeatList amount list =
  let reversed = Relude.List.reverse list in
  let rec prepend reversed acc =
    match reversed with [] -> acc | hd :: tl -> prepend tl (hd :: acc)
  in
  let rec loop amount acc =
    if amount = 0 then acc else loop (amount - 1) (prepend reversed acc)
  in
  loop amount []

let digitsAtOffsetForPhase numberFromOffset =
  let reversed = Relude.List.reverse numberFromOffset in
  let rec loop remainingDigits acc prev =
    match remainingDigits with
    | [] -> acc
    | hd :: tl ->
        let sum = prev + hd |> lastDigit in
        loop tl (sum :: acc) sum
  in
  loop reversed [] 0

let digitsAtOffset amount repition offset number =
  let repeated = repeatList repition number in
  let fromOffset = Relude.List.drop offset repeated in
  let rec loop amount number =
    if amount > 0 then loop (amount - 1) (digitsAtOffsetForPhase number)
    else number
  in
  loop amount fromOffset

let input = InputLoader.intList 16

let _ =
  input
  |> StackSafeFuture.tap (fun number ->
         phases 100 (Relude.List.length number) number
         |> Relude.List.take 8
         |> Relude.List.map string_of_int
         |> Relude.List.String.join |> Js.log2 "First 8")

let _ =
  input
  |> StackSafeFuture.tap (fun number ->
         digitsAtOffset 100 10000
           ( Relude.List.take 7 number
           |> Relude.List.map string_of_int
           |> Relude.List.String.join |> int_of_string )
           number
         |> Relude.List.take 8
         |> Relude.List.map string_of_int
         |> Relude.List.String.join |> Js.log2 "Offset 8")

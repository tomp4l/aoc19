module Droid = struct
  let output s =
    let i = s |> int_of_string in
    if i < 256 then i |> Char.chr |> String.make 1 |> Coord.write else Js.log i

  let strToIntcode s = s.[0] |> Char.code |> string_of_int

  let run intcode =
    let nextCommand = ref [] in
    let nextInput () =
      match !nextCommand with
      | [] ->
          let readline = Readline.make () in
          Readline.question readline "input dear human\n"
          |> StackSafeFuture.tap (fun _ -> Readline.close readline)
          |> StackSafeFuture.map (fun s ->
                 match Relude.String.toList s with
                 | [] -> "10"
                 | h :: tl ->
                     nextCommand :=
                       Relude.List.append "10" (Relude.List.map strToIntcode tl);
                     strToIntcode h)
      | h :: tl ->
          nextCommand := tl;
          StackSafeFuture.pure h
    in

    Intcode.run ~nextOutput:output ~nextInput intcode
end

let input = InputLoader.commaSeparated 25

let _ = StackSafeFuture.flatMap Droid.run input

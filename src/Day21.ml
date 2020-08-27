module SpringDroid = struct
  let output s =
    let i = s |> int_of_string in
    if i < 256 then i |> Char.chr |> String.make 1 |> Coord.write else Js.log i

  let assessHull code intcode =
    let remainingCode = ref (Relude.String.splitAsList ~delimiter:"" code) in

    let nextInput () =
      match !remainingCode with
      | c :: r ->
          remainingCode := r;
          StackSafeFuture.pure (c.[0] |> Char.code |> string_of_int)
      | [] -> StackSafeFuture.pure "10"
    in
    Intcode.run ~nextOutput:output ~nextInput intcode
end

let input = InputLoader.commaSeparated 21

let _ =
  input
  |> StackSafeFuture.flatMap
       (SpringDroid.assessHull {j|OR D J
NOT C T
AND T J
NOT A T
OR T J
WALK|j})
  |> StackSafeFuture.tap Js.log

let _ =
  input
  |> StackSafeFuture.flatMap
       (SpringDroid.assessHull
          {j|OR D J
AND H J
OR B T
AND C T
NOT T T
AND T J
NOT A T
OR T J
RUN|j})
  |> StackSafeFuture.tap Js.log

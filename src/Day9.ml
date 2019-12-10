let input = InputLoader.commaSeparated 9

let _ =
  input
  |> StackSafeFuture.map
       (Intcode.run ~nextInput:(fun () -> StackSafeFuture.pure "1"))

let _ =
  input
  |> StackSafeFuture.map
       (Intcode.run ~nextInput:(fun () -> StackSafeFuture.pure "2"))

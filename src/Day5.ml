let input = InputLoader.commaSeparated 5

let _ =
  input
  |> StackSafeFuture.map
       (Intcode.run ~nextInput:(fun () -> StackSafeFuture.pure "1"))

let _ =
  input
  |> StackSafeFuture.map
       (Intcode.run ~nextInput:(fun () -> StackSafeFuture.pure "5"))

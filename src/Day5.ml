let ( >> ) = Relude.Function.Infix.( >> )

let input = InputLoader.commaSeparated 5

let _ = input |> StackSafeFuture.map Intcode.run

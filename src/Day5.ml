let ( >> ) = Relude.Function.Infix.( >> )

let input = InputLoader.commaSeparatedInts 5

let _ = input |> StackSafeFuture.map Intcode.run

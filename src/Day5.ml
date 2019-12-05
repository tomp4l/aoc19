let ( >> ) = Relude.Function.Infix.( >> )

let input = InputLoader.commaSeparatedInts 5

let _ = input |. Future.map Intcode.run

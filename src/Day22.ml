module Modulo = struct
  let mul a b m =
    let maxInt32 = Int64.of_int32 Int32.max_int in
    let lessThan32Bit a = Int64.compare (Int64.abs a) maxInt32 < 1 in
    if lessThan32Bit a && lessThan32Bit b then Int64.rem (Int64.mul a b) m
    else
      let rec aux am bm r =
        if Int64.equal Int64.zero bm then r
        else
          let add =
            if Int64.equal Int64.one (Int64.logand Int64.one bm) then am
            else Int64.zero
          in
          let am' = Int64.rem (Int64.shift_left am 1) m in
          let bm' = Int64.shift_right bm 1 in
          let r' = Int64.rem (Int64.add add r) m in
          aux am' bm' r'
      in
      aux (Int64.rem a m) (Int64.rem b m) Int64.zero

  let pow a n m =
    let rec aux a n r =
      if Int64.equal Int64.zero n then r
      else
        let r' =
          if Int64.equal Int64.one (Int64.logand Int64.one n) then mul r a m
          else r
        in
        let n' = Int64.shift_right n 1 in
        let a' = mul a a m in
        aux a' n' r'
    in
    aux a n Int64.one

  (** This only works if m is prime, Fermats’s little theorem *)
  let inv_prime a m = pow a (Int64.sub m (Int64.of_string "2")) m
end

module Deal = struct
  let dealIntoNewStack = Relude.List.reverse

  let cutNCards n deck =
    let x = if n >= 0 then n else Relude.List.length deck + n in
    let start = Relude.List.take x deck in
    let rest = Relude.List.drop x deck in
    Relude.List.concat rest start

  let dealWithIncrementN n deck =
    let length = Relude.List.length deck in
    let start = Relude.Array.makeWithIndex length (fun _ -> None) in
    let rec loop remaining index shuffled =
      match remaining with
      | [] ->
          Relude.Array.mapOption Relude.Function.identity shuffled
          |> Relude.Array.toList
      | h :: t ->
          let _ = Relude.Array.setAt index (Some h) shuffled in
          let i = index + n in
          let ii = if i >= length then i - length else i in
          loop t ii shuffled
    in
    loop deck 0 start

  let factory n =
    let rec loop acc n = if n >= 0 then loop (n :: acc) (n - 1) else acc in
    loop [] (n - 1)

  let runProgram n lines =
    let deck = factory n in
    let rec loop l d =
      match l with
      | [] -> d
      | h :: t ->
          let newD =
            if h == "deal into new stack" then dealIntoNewStack d
            else if Relude.String.startsWith ~search:"cut" h then
              let n =
                Relude.String.removeFirst ~search:"cut " h |> int_of_string
              in
              cutNCards n d
            else
              let n =
                Relude.String.removeFirst ~search:"deal with increment " h
                |> int_of_string
              in
              dealWithIncrementN n d
          in
          loop t newD
    in
    loop lines deck

  let moduloProgram size shuffles position lines =
    let rec loop l a b =
      match l with
      | [] -> (a, b)
      | h :: t ->
          let newA, newB =
            if h == "deal into new stack" then
              (Int64.sub size a, Int64.rem (Int64.add size (Int64.sub b a)) size)
            else if Relude.String.startsWith ~search:"cut" h then
              let n =
                Relude.String.removeFirst ~search:"cut " h
                |> Int64.of_string |> Int64.add size
              in
              (a, Int64.rem (Int64.add b (Modulo.mul n a size)) size)
            else
              let n =
                Relude.String.removeFirst ~search:"deal with increment " h
                |> Int64.of_string
              in
              let z = Modulo.inv_prime n size in
              (Modulo.mul a z size, b)
          in
          loop t newA newB
    in
    let rec pow a b m n =
      if Int64.equal m Int64.zero then (Int64.one, Int64.zero)
      else if Int64.equal (Int64.rem m (Int64.of_string "2")) Int64.zero then
        pow (Modulo.mul a a n)
          (Int64.rem (Int64.add b (Modulo.mul a b n)) n)
          (Int64.div m (Int64.of_string "2"))
          n
      else
        let c, d = pow a b (Int64.sub m Int64.one) n in
        (Modulo.mul a c n, Int64.rem (Int64.add b (Modulo.mul a d n)) n)
    in
    let a, b = loop lines Int64.one Int64.zero in
    let a', b' = pow a b shuffles size in
    Int64.rem (Int64.add (Int64.mul position a') b') size
end

let input = InputLoader.newlineSeparated 22

let _ =
  input
  |> StackSafeFuture.tap (fun l ->
         Deal.runProgram 10007 l |> Relude.List.Int.indexOf 2019 |> Js.log)

let _ =
  input
  |> StackSafeFuture.tap (fun l ->
         Deal.moduloProgram
           (Int64.of_string "119315717514047")
           (Int64.of_string "101741582076661")
           (Int64.of_string "2020") l
         |> Int64.to_string |> Js.log)

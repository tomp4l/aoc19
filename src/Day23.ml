let ( >> ) = Relude.Function.flipCompose

module Network = struct
  let nextInput halt sendNet i nextY inputs idleNetworks lastNatY () =
    if !halt then StackSafeFuture.never ()
    else if !sendNet then
      let _ = sendNet := false in
      StackSafeFuture.pure (string_of_int i)
    else
      match !nextY with
      | Some y ->
          nextY := None;
          StackSafeFuture.pure y
      | None -> (
          let nextInput = Relude.Int.Map.get i !inputs in
          match nextInput with
          | Some (x :: y :: newIs) ->
              Relude.Array.setAt i false idleNetworks |> ignore;
              nextY := Some y;
              inputs := Relude.Int.Map.set i newIs !inputs;
              StackSafeFuture.pure x
          | _ ->
              if
                i == 0 && Relude.Array.all Relude.Function.identity idleNetworks
              then
                let nextInput = Relude.Int.Map.get 255 !inputs in
                match nextInput with
                | Some (x :: y :: _) ->
                    Relude.Array.setAt i false idleNetworks |> ignore;
                    nextY := Some y;
                    inputs := Relude.Int.Map.set 255 [] !inputs;
                    if !lastNatY = Some y then (
                      halt := true;
                      Js.log ("double nat", y) );
                    lastNatY := Some y;
                    StackSafeFuture.pure x
                | _ -> StackSafeFuture.pure "-1"
              else (
                Relude.Array.setAt i true idleNetworks |> ignore;
                StackSafeFuture.pure "-1" ) )

  let nextOutput currentOut inputs xOut useNat halt o =
    match !currentOut with
    | None -> currentOut := Some (int_of_string o)
    | Some recipient ->
        let is = Relude.Int.Map.getOrElse recipient [] !inputs in
        let newIs = Relude.List.append o is in
        inputs := Relude.Int.Map.set recipient newIs !inputs;
        if !xOut then xOut := false
        else (
          if recipient == 255 then
            if useNat then
              let ow =
                newIs |> Relude.List.drop (Relude.List.length newIs - 2)
              in
              inputs := Relude.Int.Map.set recipient ow !inputs
            else (
              halt := true;
              Js.log ("255 sent", o) );
          xOut := true;
          currentOut := None )

  let runNetwork useNat intCode =
    let idleNetworks =
      Relude.Array.makeWithIndex 50 (Relude.Function.const true)
    in
    let inputs = ref (Relude.Int.Map.make ()) in
    let halt = ref false in
    let rec makeComputers i =
      if i == 50 then ()
      else
        let nextY = ref None in
        let currentOut = ref None in
        let xOut = ref true in
        let sendNet = ref true in
        let lastNatY = ref None in
        Intcode.run intCode
          ~nextInput:
            (nextInput halt sendNet i nextY inputs idleNetworks lastNatY)
          ~nextOutput:(nextOutput currentOut inputs xOut useNat halt)
        |> ignore;
        makeComputers (i + 1)
    in
    makeComputers 0
end

let input = InputLoader.commaSeparated 23

let _ = input |> StackSafeFuture.tap (Network.runNetwork false)

let _ = input |> StackSafeFuture.tap (Network.runNetwork true)

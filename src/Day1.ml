let fuelRequired mass = (mass / 3) - 2 |> Relude.Int.max 0

let totalFuelRequired masses =
  Relude.List.map fuelRequired masses |> Relude.List.Int.sum

let rec fuelForFuel mass =
  let extraFuel = fuelRequired mass in
  if extraFuel == 0 then 0 else extraFuel + fuelForFuel extraFuel

let fuelRequiredIncludingFuel mass =
  let fuelReqired = fuelRequired mass in
  fuelReqired + fuelForFuel fuelReqired

let totalFuelRequiredIncludingFuel masses =
  Relude.List.map fuelRequiredIncludingFuel masses |> Relude.List.Int.sum

let input = InputLoader.newlineSeparatedInts 1

let ( >> ) = Relude.Function.Infix.( >> )

let _ =
  input |. Future.tap (totalFuelRequired >> Js.log2 "Fuel without extra fuel")

let _ =
  input
  |. Future.tap
       (totalFuelRequiredIncludingFuel >> Js.log2 "Fuel with fuel for fuel")

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

let input = InputLoader.loadDayAsList 1

let _ =
  input
  |. Future.tap
       Relude.Function.Infix.(
         Relude.List.map int_of_string
         >> totalFuelRequired
         >> Js.log2 "Fuel without extra fuel")

let _ =
  input
  |. Future.tap
       Relude.Function.Infix.(
         Relude.List.map int_of_string
         >> totalFuelRequiredIncludingFuel
         >> Js.log2 "Fuel with fuel for fuel")

let ( >> ) = Relude.Function.flipCompose

module Factory = struct
  type chemicalAmount = { amount : int; chemical : string }

  type recipe = { amountMade : int; ingredients : chemicalAmount list }

  exception ParseError of string

  let parseItem item =
    match Relude.String.splitList ~delimiter:" " item with
    | [ amount; chemical ] -> { amount = int_of_string amount; chemical }
    | _ -> raise (ParseError item)

  let parseInputLine line =
    let ingredientProduct = Relude.String.splitList ~delimiter:" => " line in
    match ingredientProduct with
    | [ ingredients; product ] ->
        let splitIngredients =
          Relude.String.splitList ~delimiter:", " ingredients
        in
        let ingredients = Relude.List.map parseItem splitIngredients in
        let { amount; chemical } = parseItem product in
        let recipe = { amountMade = amount; ingredients } in
        (chemical, recipe)
    | _ -> raise (ParseError line)

  let parseInputLines lines =
    lines |> Relude.List.map parseInputLine |> Relude.StringMap.fromList

  let getRecipe recipeList item =
    recipeList |> Relude.StringMap.get item |> Relude.Option.getOrThrow

  let findOreCost amount item recipeList =
    let ( + ) = Int64.add in
    let ( * ) = Int64.mul in
    let ( - ) = Int64.sub in
    let ( / ) = Int64.div in
    let rem = Int64.rem in
    let ( !! ) = Int64.of_int in

    let rec loop amountNeeded ingredients leftOvers =
      match ingredients with
      | [] -> (!!0, leftOvers)
      | { chemical = "ORE"; amount } :: rest ->
          let a, leftOvers = loop amountNeeded rest leftOvers in
          ((!!amount * amountNeeded) + a, leftOvers)
      | { chemical; amount } :: rest ->
          let amount = !!amount * amountNeeded in
          let { amountMade; ingredients } = getRecipe recipeList chemical in
          let amountMade = !!amountMade in
          let leftOverAmount =
            leftOvers |> Relude.StringMap.getOrElse chemical !!0
          in
          let newLeftOverAmount, newAmount =
            if leftOverAmount >= amount then (leftOverAmount - amount, !!0)
            else (!!0, amount - leftOverAmount)
          in
          let requiredBatches =
            if rem newAmount amountMade = !!0 then newAmount / amountMade
            else (newAmount / amountMade) + !!1
          in
          let newLeftOverAmount =
            newLeftOverAmount + ((requiredBatches * amountMade) - newAmount)
          in
          let newLeftOvers =
            Relude.StringMap.set chemical newLeftOverAmount leftOvers
          in
          let ingredientCost, newLeftOvers =
            loop requiredBatches ingredients newLeftOvers
          in
          let restCost, newLeftOvers = loop amountNeeded rest newLeftOvers in
          (restCost + ingredientCost, newLeftOvers)
    in
    let { ingredients } = getRecipe recipeList item in
    loop amount ingredients (Relude.StringMap.make ())
end

let findFuelAmount baseCost recipeList =
  let ( / ) = Int64.div in
  let ( - ) = Int64.sub in
  let ( + ) = Int64.add in
  let ore = Int64.of_string "1000000000000" in
  let worstCase = ore / baseCost in
  let rec loop amount =
    let cost, _ = Factory.findOreCost amount "FUEL" recipeList in
    if cost >= ore then amount - Int64.one
    else
      let leftOver = ore - cost in
      let worstCase = leftOver / baseCost in
      if worstCase < Int64.one then loop (Int64.one + amount)
      else loop (worstCase + amount)
  in
  loop worstCase

let input = InputLoader.newlineSeparated 14

let _ =
  input
  |> StackSafeFuture.map (fun i ->
         i |> Factory.parseInputLines |> Factory.findOreCost Int64.one "FUEL"
         |> fun (a, _) -> a)
  |> StackSafeFuture.tap (Int64.to_string >> Js.log2 "Ore cost")
  |> StackSafeFuture.map2
       (fun i v -> findFuelAmount v (Factory.parseInputLines i))
       input
  |> StackSafeFuture.tap (Int64.to_string >> Js.log2 "Fuel made")

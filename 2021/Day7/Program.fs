open System

type Input = int []

let parseInput (text: string) =
    text
    |> String.splitByChar ','
    |> Array.map int

let computeMinMax (input: Input) =
    let minValue = input |> Array.min
    let maxValue = input |> Array.max
    minValue, maxValue

let computeFuelCostAt getCost index (input: Input) =
    let mutable fuelCost = 0
    for i = 0 to input.Length - 1 do
        let cost = getCost(Math.Abs (index - input.[i]))
        fuelCost <- fuelCost + cost
    fuelCost

let solution getCost (input: Input) =
    let minValue, maxValue = computeMinMax input
    let mutable bestIndex = -1
    let mutable bestFuelCost = -1
    for i = minValue to maxValue do
        let fuelCost = computeFuelCostAt getCost i input
        // printfn "Cost at %d: %d" i fuelCost
        if bestIndex = -1 || fuelCost < bestFuelCost then
            bestFuelCost <- fuelCost
            bestIndex <- i
    bestFuelCost

let step1 (input: Input) =
    solution id input

let step2 (input: Input) =
    solution (fun i -> i * (i + 1) / 2) input

let main () =
    let input = Input.parseInputText () |> parseInput

    let n = input.Length

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1: %d" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2: %d" result2

main ()

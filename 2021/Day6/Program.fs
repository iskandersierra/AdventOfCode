open System

type Input = int []

let parseInput (text: string) =
    text
    |> String.splitByChar ','
    |> Array.map int

let growPopulationNaive (population: ResizeArray<int>) =
    let size = population.Count
    for i = 0 to size - 1 do
        let current = population.[i]
        if current = 0 then
            population.[i] <- 6
            population.Add 8
        else
            population.[i] <- current - 1

let step1Naive (input: Input) =
    let population = ResizeArray(input)
    for day = 1 to 80 do
        growPopulationNaive population
    population.Count

let growPopulation (population: uint64[]) =
    let newPopulation = Array.zeroCreate 9
    newPopulation.[0] <- population.[1]
    newPopulation.[1] <- population.[2]
    newPopulation.[2] <- population.[3]
    newPopulation.[3] <- population.[4]
    newPopulation.[4] <- population.[5]
    newPopulation.[5] <- population.[6]
    newPopulation.[6] <- population.[7] + population.[0]
    newPopulation.[7] <- population.[8]
    newPopulation.[8] <- population.[0]
    Array.blit newPopulation 0 population 0 9

let solution totalDays (input: Input) =
    let population = Array.zeroCreate<uint64> 9

    for i = 0 to input.Length - 1 do
        let value = input.[i]
        population.[value] <- population.[value] + 1UL

    for day = 1 to totalDays do
        growPopulation population

    population |> Array.sum

let step1 (input: Input) =
    solution 80 input

let step2 (input: Input) =
    solution 256 input

let main () =
    let input = Input.parseInputText () |> parseInput

    let n = input.Length

    let result1 = Time.measureN n (fun () -> step1Naive input)
    printfn "***** STEP1 Naive: %d" result1

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1: %d" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2: %d" result2

main ()

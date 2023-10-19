open System

type Input =
    {
        width: int
        instructions: uint32[]
    }

let parseLine (line: string) = Convert.ToUInt32(line, 2)

let parseInput (lines: string[]) =
    let width = lines.[0].Length
    let instructions = Array.map parseLine lines
    { width = width; instructions = instructions }

let step1 input =
    let size = Array.length input.instructions
    let mutable gammaRate = 0u
    let mutable epsilonRate = 0u
    for i = 0 to input.width - 1 do
        let mutable count1 = 0
        let bitMask = 1u <<< i
        for j = 0 to size - 1 do
            let isOne = (input.instructions.[j] &&& bitMask) <> 0u
            if isOne then
                count1 <- count1 + 1
        if count1 >= size - count1 then
            gammaRate <- gammaRate ||| bitMask
        else
            epsilonRate <- epsilonRate ||| bitMask
    gammaRate * epsilonRate

let findBit mostCommon defaultBit bitMask size (instructions: uint32[]) =
    let mutable count1 = 0
    for i = 0 to size - 1 do
        let isOne = (instructions.[i] &&& bitMask) <> 0u
        if isOne then
            count1 <- count1 + 1
    if mostCommon && count1 > size - count1 || not mostCommon && count1 < size - count1 then
        1u
    elif mostCommon && count1 < size - count1 || not mostCommon && count1 > size - count1 then
        0u
    else
        defaultBit

let keepWithBit bit bitMask size (instructions: uint32[]) =
    let mutable j = 0
    for i = 0 to size - 1 do
        let isOne = (instructions.[i] &&& bitMask) <> 0u
        if isOne = (bit = 1u) then
            instructions.[j] <- instructions.[i]
            j <- j + 1
    j

let findRate mostCommon defaultBit input =
    let mutable size = Array.length input.instructions
    let instructions = Array.copy input.instructions
    let mutable position = input.width - 1
    while size > 0 && position >= 0 do
        let bitMask = 1u <<< position
        let bit = findBit mostCommon defaultBit bitMask size instructions
        size <- keepWithBit bit bitMask size instructions
        position <- position - 1
    instructions.[0]

let step2 input =
    let oxygenRate = findRate true 1u input
    let co2ScrubberRate = findRate false 0u input
    oxygenRate * co2ScrubberRate

let main() =
    let input =
        Input.parseInputLines()
        |> parseInput

    let n = Array.length input.instructions * input.width

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1: %d" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2: %d" result2

main()

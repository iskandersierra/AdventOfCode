open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input =
    { minX: int
      maxX: int
      minY: int
      maxY: int }

let inputRegex =
    Regex(
        @"^target area: x=(?<minX>(\-)?\d+)\.\.(?<maxX>(\-)?\d+), y=(?<minY>(\-)?\d+)\.\.(?<maxY>(\-)?\d+)$",
        RegexOptions.Compiled
    )

let parseInput (text: string) =
    match inputRegex.Match(text) with
    | m when m.Success ->
        let minX = int m.Groups.["minX"].Value
        let maxX = int m.Groups.["maxX"].Value
        let minY = int m.Groups.["minY"].Value
        let maxY = int m.Groups.["maxY"].Value

        { minX = minX
          maxX = maxX
          minY = minY
          maxY = maxY }

    | _ -> failwithf "Invalid input: %s" text


let minSpeedToDistanceFloat distance =
    ((sqrt (8.0 * distance + 1.0)) - float 1)
    / float 2

let maxSpeedToDistanceFloat distance = (distance + 1.0) / 2.0

let hitsTarget vx vy input =
    let rec loop vx vy x y =
        if y < input.minY || x > input.maxX then
            false
        elif x >= input.minX
             && x <= input.maxX
             && y >= input.minY
             && y <= input.maxY then
            true
        else
            let x = x + vx
            let y = y + vy
            let vx = max 0 (vx - 1)
            let vy = vy - 1
            loop vx vy x y

    loop vx vy 0 0

let findSpeedRange rangeHigh rangeLow vx input =
    let mutable count = 0

    for vy = rangeLow to rangeHigh do
        let hit = hitsTarget vx vy input

        if hit then
            count <- count + 1

    count

let step1 (input: Input) =
    let maxVY = -(1 + input.minY)
    let maxHeight = maxVY * (maxVY + 1) / 2
    maxHeight

let step2 (input: Input) =
    let minVX =
        minSpeedToDistanceFloat input.minX |> ceil |> int

    let maxVX =
        float (input.maxX + 1) / 2.0 |> floor |> int

    let rangeHigh = -(1 + input.minY)

    let rangeLow =
        -(float (-input.minY - 1) / 2.0 |> floor |> int)

    // printf "minVX: %d\n" minVX
    // printf "maxVXShort: %d\n" maxVXShort
    // printf "maxVX: %d\n" maxVX
    // printf "rangeHigh: %d\n" rangeHigh
    // printf "rangeLow: %d\n" rangeLow
    let targetWidth = input.maxX - input.minX + 1
    let targetHeight = input.maxY - input.minY + 1
    let targetSize = targetWidth * targetHeight
    // printfn "targetSize: %d" targetSize
    let mutable result = targetSize
    for vx = minVX to maxVX do
        let count = findSpeedRange rangeHigh rangeLow vx input
        // printfn "- for VX=%d, speed range size is %d. Searched between %d and %d below" vx count rangeHigh rangeLow
        result <- result + count
    // printfn ""
    result

let main () =
    let input = Input.parseInputText () |> parseInput
    // printfn "Input:\n%A" input

    let n = 1

    (fun () -> step1 input)
    |> Time.measureN n
    |> printfn "***** STEP1:\n%O"

    (fun () -> step2 input)
    |> Time.measureN n
    |> printfn "***** STEP2:\n%O"


main ()

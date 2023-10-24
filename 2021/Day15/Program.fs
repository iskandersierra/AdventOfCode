open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open Spectre.Console

type Input = int [,]

and [<Struct>] Position = { x: int; y: int }

and Output1 =
    { result: int }
    override this.ToString() = $"Result: {this.result}"

let parseInput (text: string []) =
    text
    |> Array.map (fun line ->
        line
        |> Seq.map (fun c -> int c - int '0')
        |> Seq.toArray)
    |> (fun arr ->
        let width = arr.[0].Length
        let height = arr.Length
        Array2D.init height width (fun y x -> arr.[y].[x]))

let augmentInput size input =
    let height = Array2D.length1 input
    let width = Array2D.length2 input
    let bigHeight = height * size
    let bigWidth = width * size
    let bigInput = Array2D.zeroCreate bigHeight bigWidth
    for i = 0 to size - 1 do
        for j = 0 to size - 1 do
        let increment = i + j
        for y = 0 to height - 1 do
            for x = 0 to width - 1 do
                let bigY = y + i * height
                let bigX = x + j * width
                let value = input.[y, x]
                let bigValue = (value + increment - 1) % 9 + 1
                bigInput.[bigY, bigX] <- bigValue
    bigInput


let around =
    [| { x = -1; y = 0 }
       { x = 1; y = 0 }
       { x = 0; y = -1 }
       { x = 0; y = 1 } |]

let solve (input: Input) =
    let height = Array2D.length1 input
    let width = Array2D.length2 input
    let exitPos = { x = width - 1; y = height - 1 }

    let isInside pos =
        pos.x >= 0
        && pos.x < width
        && pos.y >= 0
        && pos.y < height

    let costMap = Array2D.zeroCreate height width
    Array2D.clearWith -1 costMap
    costMap.[0, 0] <- 0

    let queue = PriorityQueue()
    queue.Enqueue({ x = 0; y = 0 }, 0)

    let mutable found = false

    while not found && queue.Count > 0 do
        let pos = queue.Dequeue()
        let cost = costMap.[pos.y, pos.x]

        if pos = exitPos then
            found <- true
        else
            for i = 0 to around.Length - 1 do
                let newPos =
                    { x = pos.x + around.[i].x
                      y = pos.y + around.[i].y }

                if isInside newPos then
                    let currentCost = costMap.[newPos.y, newPos.x]
                    let newCost = cost + input.[newPos.y, newPos.x]

                    if currentCost = -1 || newCost < currentCost then
                        queue.Enqueue(newPos, newCost)
                        costMap.[newPos.y, newPos.x] <- newCost

    costMap.[exitPos.y, exitPos.x]

let step1 (input: Input) = solve input

let step2 (input: Input) = solve input


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input

    let n =
        (Array2D.length1 input) * (Array2D.length2 input)

    let result1 = Time.measureN n (fun () -> step1 input)

    printfn "***** STEP1:\n%O" result1

    let input = augmentInput 5 input

    let n =
        (Array2D.length1 input) * (Array2D.length2 input)

    let result2 = Time.measureN n (fun () -> step2 input)

    printfn "***** STEP2:\n%O" result2

main ()

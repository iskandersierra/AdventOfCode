open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = byte [,]

let side = 10

type Output1 =
    { flashCount: int
      generations: Generation [] }
    override this.ToString() =
        let sb = new StringBuilder()

        sb
            .Append("Total flash count: ")
            .Append(this.flashCount)
            .AppendLine()
        |> ignore

        sb.ToString()

and Generation = {
    output: Input
    flashCount: int
}
with
    override this.ToString() =
        let sb = new StringBuilder()

        sb.AppendLine($"Flash count: {this.flashCount}")
        |> ignore

        for i = 0 to side - 1 do
            for j = 0 to side - 1 do
                sb.Append(this.output.[i, j]) |> ignore

            sb.AppendLine() |> ignore

        sb.ToString()

type Output2 =
    { firstFullFlash: int }
    override this.ToString() =
        let sb = new StringBuilder()

        sb
            .Append("First full flash: ")
            .Append(this.firstFullFlash)
            .AppendLine()
        |> ignore

        sb.ToString()

let parseInput (text: string []) : Input =
    text
    |> Array.map (fun line ->
        Seq.map (fun ch -> byte ch - byte '0') line
        |> Seq.toArray)
    |> fun lines -> Array2D.init lines.Length lines.[0].Length (fun i j -> lines.[i].[j])

let increaseGeneration flashes (input: Input) =
    let queue = Queue()
    let input = Array2D.copy input

    let affectArea i j =
        let minX = max 0 (i - 1)
        let maxX = min (side - 1) (i + 1)
        let minY = max 0 (j - 1)
        let maxY = min (side - 1) (j + 1)

        for a = minX to maxX do
            for b = minY to maxY do
                if (a <> i || b <> j)
                   && Array2D.get flashes a b = false then
                    queue.Enqueue((a, b))

    let increasePoint i j =
        let value = Array2D.get input i j

        let newValue =
            if value = 9uy then
                Array2D.set flashes i j true
                affectArea i j
                0uy
            elif Array2D.get flashes i j = false then
                value + 1uy
            else
                value

        Array2D.set input i j newValue

    for i = 0 to side - 1 do
        for j = 0 to side - 1 do
            increasePoint i j

    while queue.Count > 0 do
        let (i, j) = queue.Dequeue()
        increasePoint i j

    input

let solve genCount (input: Input) =
    let generations = ResizeArray()
    generations.Add({ output = input; flashCount = 0 })
    let mutable currentInput = input
    let mutable index = 1
    while index <= genCount do
        let flashes = Array2D.create side side false
        let nextInput = increaseGeneration flashes currentInput
        let flashCount = flashes |> Array2D.countBy (fun x -> x)
        generations.Add({ output = nextInput; flashCount = flashCount })
        index <- index + 1
        currentInput <- nextInput

    let generations = generations.ToArray()
    let flashCount = generations |> Array.sumBy (fun g -> g.flashCount)

    { Output1.flashCount = flashCount; generations = generations }


let step1 (input: Input) = solve 100 input

let step2 (input: Input) =
    let mutable currentInput = input
    let mutable index = 1
    let mutable found = false
    let flashes = Array2D.create side side false
    while not found do
        Array2D.clearWith false flashes
        let nextInput = increaseGeneration flashes currentInput
        let flashCount = flashes |> Array2D.countBy (fun x -> x)
        found <- flashCount = 100
        if not found then
            index <- index + 1
            currentInput <- nextInput

    { Output2.firstFullFlash = index }


let main () =
    let input = Input.parseInputLines () |> parseInput

    let n = side * side

    let result1 =
        Time.measureN (n * 100) (fun () -> step1 input)
    printfn "***** STEP1:\n%O" result1

    let result2 = Time.measureN (n * 1000) (fun () -> step2 input)
    printfn "***** STEP2:\n%O" result2

main ()

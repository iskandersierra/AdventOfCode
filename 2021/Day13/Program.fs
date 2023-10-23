open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = { paper: Paper; folds: Fold [] }

and [<Struct>] Point = { x: int; y: int }
and [<Struct>] Fold = { horizontal: bool; position: int }

and Paper =
    { points: Point [] }
    member this.Optimize() =
        let minX =
            this.points |> Seq.map (fun p -> p.x) |> Seq.min

        let maxX =
            this.points |> Seq.map (fun p -> p.x) |> Seq.max

        let minY =
            this.points |> Seq.map (fun p -> p.y) |> Seq.min

        let maxY =
            this.points |> Seq.map (fun p -> p.y) |> Seq.max

        let points = this.points |> HashSet

        { OptimPaper.points = points
          minX = minX
          maxX = maxX
          minY = minY
          maxY = maxY }

    override this.ToString() = this.Optimize().ToString()

and OptimPaper =
    { points: HashSet<Point>
      minX: int
      maxX: int
      minY: int
      maxY: int }
    member this.Dots = this.points.Count
    member this.Width = this.maxX - this.minX + 1
    member this.Height = this.maxY - this.minY + 1
    member this.Size = this.Width * this.Height

    override this.ToString() =
        let sb = new StringBuilder()

        for y = this.minY to this.maxY do
            for x = this.minX to this.maxX do
                let c =
                    if this.points.Contains { x = x; y = y } then
                        '█'
                    else
                        ' '

                sb.Append(c) |> ignore

            sb.AppendLine() |> ignore

        sb.ToString()

type OptimInput1 = { paper: OptimPaper; folds: Fold [] }

let optimizeInput1 (input: Input) =
    let paper = input.paper.Optimize()
    { paper = paper; folds = input.folds }

type Output1 =
    { result: int
      papers: OptimPaper [] }
    override this.ToString() =
        let sb = new StringBuilder()

        this.papers
        |> Array.iteri (fun i paper ->
            sb.AppendLine($"Paper {i + 1} ({paper.Width} x {paper.Height} = {paper.Size}) with {paper.Dots} dots:")
            |> ignore
            if paper.Width <= 80 && paper.Height <= 20 then
                sb.AppendLine(paper.ToString()) |> ignore
            sb.AppendLine() |> ignore)

        sb.AppendLine($"Result: {this.result}") |> ignore

        sb.ToString()

let pointRegex =
    Regex($@"^(?<x>\d+),(?<y>\d+)$", RegexOptions.Compiled)

let foldRegex =
    Regex($@"^fold along (?<axis>x|y)=(?<pos>\d+)$", RegexOptions.Compiled)

let parseInput (text: string []) : Input =
    let points = ResizeArray()
    let folds = ResizeArray()

    for i = 0 to text.Length - 1 do
        let line = text.[i]

        match pointRegex.Match line with
        | m when m.Success ->
            let x = int m.Groups.["x"].Value
            let y = int m.Groups.["y"].Value
            points.Add({ x = x; y = y })
        | _ ->
            match foldRegex.Match line with
            | m when m.Success ->
                let axis = m.Groups.["axis"].Value
                let pos = int m.Groups.["pos"].Value
                let horizontal = axis = "y"

                folds.Add(
                    { horizontal = horizontal
                      position = pos }
                )
            | _ ->
                match line with
                | "" -> ()
                | _ -> failwithf "Invalid input line: %s. Expecting a point or a fold." line

    let points = points.ToArray()
    let folds = folds.ToArray()

    { Input.paper = { points = points }
      folds = folds }

let foldPaper (fold: Fold) (paper: OptimPaper) =
    match fold.horizontal with
    | true ->
        paper.points
        |> Seq.map (fun p ->
            if p.y >= fold.position then
                let distance = p.y - fold.position
                let newY = fold.position - distance
                { x = p.x; y = newY }
            else
                p)
    | false ->
        paper.points
        |> Seq.map (fun p ->
            if p.x >= fold.position then
                let distance = p.x - fold.position
                let newX = fold.position - distance
                { x = newX; y = p.y }
            else
                p)
    |> Seq.toArray
    |> fun points -> { Paper.points = points }.Optimize()

let step1 (input: OptimInput1) =
    let paper = input.paper |> foldPaper input.folds.[0]
    let dotsInLastPaper = paper.Dots

    { result = dotsInLastPaper;
      papers = [| input.paper; paper |] }

let step2 (input: OptimInput1) =
    let papers = ResizeArray()
    papers.Add(input.paper)
    let mutable currentPaper = input.paper
    for fold in input.folds do
        currentPaper <- foldPaper fold currentPaper
        papers.Add(currentPaper)
    let papers = papers.ToArray()
    let dotsInLastPaper = currentPaper.Dots

    { result = dotsInLastPaper;
      papers = papers }


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%s" (input.points |> pointsToString)
    let optimInput = optimizeInput1 input
    // printfn "Optimized Input: %A" optimInput

    let n = optimInput.paper.Size

    let result1 =
        Time.measureN n (fun () -> step1 optimInput)

    printfn "***** STEP1:\n%O" result1

    let n = optimInput.paper.Size * optimInput.folds.Length

    let result2 =
        Time.measureN n (fun () -> step2 optimInput)

    printfn "***** STEP2:\n%O" result2

main ()

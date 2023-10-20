open System
open System.Text.RegularExpressions

type Input = { ventLines: VentLine [] }

and [<Struct>] VentLine =
    { x1: int
      y1: int
      x2: int
      y2: int }
    member this.MinX = min this.x1 this.x2
    member this.MaxX = max this.x1 this.x2
    member this.MinY = min this.y1 this.y2
    member this.MaxY = max this.y1 this.y2

    member this.IsHorizontal = this.y1 = this.y2
    member this.IsVertical = this.x1 = this.x2

    member this.IsDiagonalUp =
        this.x1 < this.x2 && this.y1 < this.y2
        || this.x1 > this.x2 && this.y1 > this.y2

    member this.IsDiagonalDown =
        this.x1 < this.x2 && this.y1 > this.y2
        || this.x1 > this.x2 && this.y1 < this.y2

let ventLineRegex =
    Regex(@"^(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)$", RegexOptions.Compiled)

let parseVentLine (line: string) =
    let m = ventLineRegex.Match(line)

    if not m.Success then
        failwithf "Failed to parse vent line: %s" line
    else
        let x1 = int m.Groups.["x1"].Value
        let y1 = int m.Groups.["y1"].Value
        let x2 = int m.Groups.["x2"].Value
        let y2 = int m.Groups.["y2"].Value
        { x1 = x1; y1 = y1; x2 = x2; y2 = y2 }

let parseInput (lines: string []) =
    let ventLines = lines |> Array.map parseVentLine

    { ventLines = ventLines }

let computeFloorMinMax input =
    let minX =
        input.ventLines
        |> Array.minBy (fun l -> l.MinX)
        |> fun l -> l.MinX

    let maxX =
        input.ventLines
        |> Array.maxBy (fun l -> l.MaxX)
        |> fun l -> l.MaxX

    let minY =
        input.ventLines
        |> Array.minBy (fun l -> l.MinY)
        |> fun l -> l.MinY

    let maxY =
        input.ventLines
        |> Array.maxBy (fun l -> l.MaxY)
        |> fun l -> l.MaxY

    minX, maxX, minY, maxY

let drawLine allowDiagonals minX minY (ventLine: VentLine) floor =
    if ventLine.IsHorizontal then
        let minLineX = min ventLine.x1 ventLine.x2
        let maxLineX = max ventLine.x1 ventLine.x2

        for i = 0 to maxLineX - minLineX do
            let xPos = i + minLineX - minX
            let yPos = ventLine.y1 - minY
            let value = Array2D.get floor xPos yPos
            Array2D.set floor xPos yPos (value + 1)

    elif ventLine.IsVertical then
        let minLineY = min ventLine.y1 ventLine.y2
        let maxLineY = max ventLine.y1 ventLine.y2

        for i = 0 to maxLineY - minLineY do
            let xPos = ventLine.x1 - minX
            let yPos = i + minLineY - minY
            let value = Array2D.get floor xPos yPos
            Array2D.set floor xPos yPos (value + 1)

    elif allowDiagonals && ventLine.IsDiagonalUp then
        let minLineX = min ventLine.x1 ventLine.x2
        let maxLineX = max ventLine.x1 ventLine.x2
        let minLineY = min ventLine.y1 ventLine.y2
        let maxLineY = max ventLine.y1 ventLine.y2
        assert (minLineX <> maxLineX)
        assert (maxLineX - minLineX = maxLineY - minLineY)

        for i = 0 to maxLineX - minLineX do
            let xPos = i + minLineX - minX
            let yPos = i + minLineY - minY
            let value = Array2D.get floor xPos yPos
            Array2D.set floor xPos yPos (value + 1)

    elif allowDiagonals && ventLine.IsDiagonalDown then
        let minLineX = min ventLine.x1 ventLine.x2
        let maxLineX = max ventLine.x1 ventLine.x2
        let minLineY = min ventLine.y1 ventLine.y2
        let maxLineY = max ventLine.y1 ventLine.y2
        assert (minLineX <> maxLineX)
        assert (maxLineX - minLineX = maxLineY - minLineY)

        for i = 0 to maxLineX - minLineX do
            let xPos = i + minLineX - minX
            let yPos = maxLineY - i - minY
            let value = Array2D.get floor xPos yPos
            Array2D.set floor xPos yPos (value + 1)

let countDuplicates floor =
    let mutable countDuplicates = 0

    for x = 0 to Array2D.length1 floor - 1 do
        for y = 0 to Array2D.length2 floor - 1 do
            if floor.[x, y] > 1 then
                countDuplicates <- countDuplicates + 1

    countDuplicates

let step1 input =
    let minX, maxX, minY, maxY = computeFloorMinMax input

    let floor =
        Array2D.zeroCreate (maxX - minX + 1) (maxY - minY + 1)

    for index = 0 to input.ventLines.Length - 1 do
        let ventLine = input.ventLines.[index]

        drawLine true minX minY ventLine floor

    let result = countDuplicates floor

    result

let step2 input =
    // printfn "%A" input
    0

let main () =
    let input = Input.parseInputLines () |> parseInput

    let n = input.ventLines.Length

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1: %d" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2: %d" result2

main ()

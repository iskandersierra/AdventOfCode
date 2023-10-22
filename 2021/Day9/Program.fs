open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = int [,]

type Output1 =
    { lowPoints: LowPoint []
      result: int }
    override this.ToString() =
        let sb = new StringBuilder()

        sb.AppendLine($"Low points [{this.lowPoints.Length}]:")
        |> ignore

        for lowPoint in this.lowPoints |> Seq.takeAtMost 5 do
            sb.Append("  - ").AppendLine(lowPoint.ToString())
            |> ignore

        sb
            .Append("Result: ")
            .Append(this.result)
            .AppendLine()
        |> ignore

        sb.ToString()

and LowPoint =
    { height: int
      row: int
      col: int }
    override this.ToString() =
        sprintf "%d at (%d, %d)" this.height this.row this.col

type Output2 =
    { basins: Basin []
      result: int }
    override this.ToString() =
        let sb = new StringBuilder()

        sb.AppendLine($"Basins [{this.basins.Length}]:")
        |> ignore

        for basin in this.basins |> Seq.takeAtMost 5 do
            sb.Append("  - ").AppendLine(basin.ToString())
            |> ignore

        sb
            .Append("Result: ")
            .Append(this.result)
            .AppendLine()
        |> ignore

        sb.ToString()

and Basin =
    { height: int
      size: int
      row: int
      col: int }
    override this.ToString() =
        sprintf "%d at (%d, %d) with size %d" this.height this.row this.col this.size

and BasinComparer() =
    interface IComparer<Basin> with
        member this.Compare (x: Basin, y: Basin) = y.size - x.size


let parseInput (text: string []) : Input =
    let data =
        text
        |> Array.map (fun line ->
            line
            |> Seq.map (fun ch -> int ch - int '0')
            |> Seq.toArray)

    Array2D.init data.Length data.[0].Length (fun i j -> data.[i].[j])

let solver solveLowPoint (input: Input) =
    let rows = Array2D.length1 input
    let cols = Array2D.length2 input
    let results = ResizeArray()

    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            let center = input.[i, j]
            let mutable isLowPoint = true

            if i > 0 then
                isLowPoint <- isLowPoint && center < input.[i - 1, j]

            if i < rows - 1 then
                isLowPoint <- isLowPoint && center < input.[i + 1, j]

            if j > 0 then
                isLowPoint <- isLowPoint && center < input.[i, j - 1]

            if j < cols - 1 then
                isLowPoint <- isLowPoint && center < input.[i, j + 1]

            if isLowPoint then
                results.Add(solveLowPoint i j)

    results

let clearBasinMap basinMap =
    Array2D.iteri (fun i j _ -> Array2D.set basinMap i j false) basinMap
    // for i = 0 to rows - 1 do
    //     for j = 0 to cols - 1 do
    //         basinMap.[i, j] <- false

let createBasin input basinMap i j =
    let rows = Array2D.length1 input
    let cols = Array2D.length2 input
    clearBasinMap basinMap
    let rec loop previousHeight i j count =
        if Array2D.get basinMap i j = true then
            count
        else
            let height = Array2D.get input i j
            if height >= 9 || height < previousHeight then
                count
            else
                Array2D.set basinMap i j true
                let mutable count = count + 1
                if i > 0 then
                    count <- loop height (i - 1) j count
                if i < rows - 1 then
                    count <- loop height (i + 1) j count
                if j > 0 then
                    count <- loop height i (j - 1) count
                if j < cols - 1 then
                    count <- loop height i (j + 1) count
                count

    let height = input.[i, j]
    let size = loop height i j 0
    { height = height
      size = size
      row = i
      col = j }


let step1 (input: Input) =
    let createLowPoint i j =
        { height = input.[i, j]
          row = i
          col = j }

    let results = solver createLowPoint input

    let lowPoints = results.ToArray()

    let result =
        lowPoints
        |> Array.sumBy (fun lowPoint -> lowPoint.height + 1)

    { lowPoints = lowPoints
      result = result }

let step2 (input: Input) =
    let rows = Array2D.length1 input
    let cols = Array2D.length2 input
    let basinMap = Array2D.zeroCreate rows cols

    let results = solver (createBasin input basinMap) input

    let basins = results.ToArray()
    Array.Sort(basins, BasinComparer())

    let result =
        basins
        |> Seq.takeAtMost 3
        |> Seq.fold (fun acc basin -> acc * basin.size) 1

    { basins = basins
      result = result }

let main () =
    let input = Input.parseInputLines () |> parseInput

    let n = Array2D.length1 input * Array2D.length2 input

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1:\n%O" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2:\n%O" result2

main ()

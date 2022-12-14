#r "nuget: FParsec, 1.1.1"

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open FParsec

#nowarn "0025"

type RockPosition =
    { x: int
      y: int }
    override this.ToString() = sprintf "%d,%d" this.x this.y

type RockLine =
    | RockLine of rockPositions: RockPosition list
    override this.ToString() =
        let (RockLine rockPositions) = this

        rockPositions
        |> List.map (sprintf "%O")
        |> String.concat " -> "

type CaveInput =
    | CaveInput of rockLines: RockLine list
    override this.ToString() =
        let (CaveInput rockLines) = this

        rockLines
        |> List.map (sprintf "%O")
        |> String.concat "\n"

[<Literal>]
let StartX = 500

[<Literal>]
let AirID = 0uy

[<Literal>]
let RockID = 1uy

[<Literal>]
let SandID = 2uy

type Cave =
    | Cave of cave: byte [,] * start: RockPosition * minX: int
    override this.ToString() =
        let (Cave (cave, start, minX)) = this
        let width = cave.GetLength(0)
        let height = cave.GetLength(1)
        let sb = StringBuilder()

        sb.AppendLine(sprintf "Start: %O" start)
        |> ignore

        sb.AppendLine(sprintf "Size: %d,%d" width height)
        |> ignore

        sb.AppendLine(sprintf "MinX: %d" minX) |> ignore

        for y in 0 .. height - 1 do
            for x in 0 .. width - 1 do
                if x = start.x && y = start.y then
                    sb.Append('+') |> ignore
                elif cave.[x, y] = RockID then
                    sb.Append('#') |> ignore
                elif cave.[x, y] = SandID then
                    sb.Append('o') |> ignore
                else
                    sb.Append('.') |> ignore

            sb.AppendLine() |> ignore

        sb.ToString()

let parseCaveInput lines =
    let parseRockPosition: Parser<RockPosition, unit> =
        pipe2 (pint32 .>> pchar ',') pint32 (fun x y -> { x = x; y = y })

    let parseRockLine: Parser<RockLine, unit> =
        sepBy1 parseRockPosition (pstring " -> ")
        |>> RockLine

    let parseCaveInput: Parser<CaveInput, unit> =
        (sepBy1 parseRockLine newline) .>> eof
        |>> CaveInput

    match run parseCaveInput lines with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

let toExplicitCave withFloor (CaveInput rockLines) =
    let (minX, maxX, maxY) =
        rockLines
        |> List.collect (fun (RockLine rocks) -> rocks)
        |> List.fold
            (fun (minX, maxX, maxY) rock -> (minX |> min rock.x, maxX |> max rock.x, maxY |> max rock.y))
            (Int32.MaxValue, Int32.MinValue, Int32.MinValue)

    // If the cave has a floor, we need to add an "infinite" rock line two tiles below the lowest rock line
    let (minX, maxX, maxY) =
        if withFloor then
            let floorY = maxY + 2
            // We need to add as many tiles to left and right as needed to avoid any sand from falling out of the cave
            let floorMinX = min (StartX - floorY - 1) minX
            let floorMaxX = max (StartX + floorY + 1) maxX
            (floorMinX, floorMaxX, floorY)
        else
            (minX, maxX, maxY)

    let width = maxX - minX + 1
    let height = maxY + 1
    let start = { x = StartX - minX; y = 0 }
    let cave = Array2D.zeroCreate width height

    let fillLine fromPosition toPosition =
        if fromPosition.x = toPosition.x then
            let fromY = min fromPosition.y toPosition.y
            let toY = max fromPosition.y toPosition.y

            for y in fromY .. toY do
                cave.[fromPosition.x, y] <- RockID
        elif fromPosition.y = toPosition.y then
            let fromX = min fromPosition.x toPosition.x
            let toX = max fromPosition.x toPosition.x

            for x in fromX .. toX do
                cave.[x, fromPosition.y] <- RockID
        else
            failwithf "Invalid rock line: %O -> %O" fromPosition toPosition

    for (RockLine rockLine) in rockLines do
        let length = List.length rockLine

        let rockLine =
            rockLine
            |> List.map (fun rock -> { rock with x = rock.x - minX })

        Seq.zip (Seq.take (length - 1) rockLine) (Seq.skip 1 rockLine)
        |> Seq.iter (fun (fromPosition, toPosition) -> fillLine fromPosition toPosition)

    if withFloor then
        fillLine { x = 0; y = maxY } { x = width - 1; y = maxY }

    Cave(cave, start, minX)

let dropSand (Cave (cave, start, minX)) =
    let width = cave.GetLength(0)
    let height = cave.GetLength(1)

    let rec loop pos =
        if pos.y + 1 >= height then
            None
        elif cave.[pos.x, pos.y + 1] = AirID then
            loop { x = pos.x; y = pos.y + 1 }
        elif pos.x - 1 < 0 then
            None
        elif cave.[pos.x - 1, pos.y + 1] = AirID then
            loop { x = pos.x - 1; y = pos.y + 1 }
        elif pos.x + 1 >= width then
            None
        elif cave.[pos.x + 1, pos.y + 1] = AirID then
            loop { x = pos.x + 1; y = pos.y + 1 }
        else
            Some pos

    match loop start with
    | Some pos ->
        cave.[pos.x, pos.y] <- SandID
        Some (Cave(cave, start, minX))
    | _ -> None

let dropSandUntilStable cave =
    let rec loop step cave =
        match dropSand cave with
        | Some(Cave(newCave, start, minX)) ->
            if newCave.[start.x, start.y] = SandID then
                printfn "Sand reached start position after %d steps" step
                printfn "%O" cave
                step + 1
            else
                loop (step + 1) (Cave(newCave, start, minX))
        | None ->
            printfn "Last Step %d" step
            printfn "%O" cave
            step

    loop 0 cave


let execute lines =
    let caveInput = parseCaveInput lines

    let cave = toExplicitCave false caveInput
    let stepCount = dropSandUntilStable cave
    printfn "CAVE WITHOUT FLOOR: %O" stepCount
    printfn ""

    let cave = toExplicitCave true caveInput
    let stepCount = dropSandUntilStable cave
    printfn "CAVE WITH FLOOR: %O" stepCount
    printfn ""


Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

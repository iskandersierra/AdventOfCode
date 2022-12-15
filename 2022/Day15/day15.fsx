#r "nuget: FParsec, 1.1.1"

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open FParsec

#nowarn "0025"

type Position =
    { x: int
      y: int }
    override this.ToString() = sprintf "%d,%d" this.x this.y
    static member Create x y = { x = x; y = y }

let distance p1 p2 = abs (p1.x - p2.x) + abs (p1.y - p2.y)

type ScanLine =
    | ScanLine of minX: int * maxX: int
    override this.ToString() =
        let (ScanLine (minX, maxX)) = this
        sprintf "%d -> %d" minX maxX
    member this.Length =
        let (ScanLine (minX, maxX)) = this
        maxX - minX + 1

type SensorReport =
    | SensorReport of sensor: Position * beacon: Position
    override this.ToString() =
        let (SensorReport (sensor, beacon)) = this
        sprintf "Sensor %O -> Beacon %O" sensor beacon

    static member Create sensor beacon = SensorReport(sensor, beacon)

type ProblemInput =
    | ProblemInput of lines: SensorReport list
    override this.ToString() =
        let (ProblemInput lines) = this

        lines
        |> List.map (sprintf "%O")
        |> String.concat "\n"

let parseInput lines =
    let parsePosition: Parser<Position, unit> =
        let parseX = pstring "x=" >>. pint32 .>> pstring ", "
        let parseY = pstring "y=" >>. pint32
        pipe2 parseX parseY Position.Create

    let parseSensorReport: Parser<SensorReport, unit> =
        let parseSensor = pstring "Sensor at " >>. parsePosition

        let parseBeacon =
            pstring ": closest beacon is at "
            >>. parsePosition

        pipe2 parseSensor parseBeacon SensorReport.Create

    let parseProblemInput: Parser<ProblemInput, unit> =
        (sepBy1 parseSensorReport newline) .>> eof
        |>> ProblemInput

    match run parseProblemInput lines with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

let findAllScanLines lineY (ProblemInput sensors) =
    sensors
    |> Seq.collect (fun (SensorReport (sensor, beacon)) ->
        let d = distance sensor beacon
        let lineStart = sensor.x - d + abs (sensor.y - lineY)
        let lineEnd = sensor.x + d - abs (sensor.y - lineY)
        if lineStart <= lineEnd then
            [ ScanLine(lineStart, lineEnd) ]
        else
            [])

let printScanLines lines =
    lines
    |> List.map (sprintf "%O")
    |> String.concat ", "

let mergeScanLines scanLines =
    let rec insertScanLine scanLines (ScanLine (minX, maxX)) =
        match scanLines with
        | [] ->
            // the list is empty
            [ ScanLine(minX, maxX) ]

        | ScanLine (minX', maxX') :: rest when minX > maxX' ->
            // the first line in the list is completely before the new line
            let rest' =
                insertScanLine rest (ScanLine(minX, maxX))

            ScanLine(minX', maxX') :: rest'

        | ScanLine (minX', _) :: _ when maxX < minX' ->
            // the first line in the list is completely after the new line
            ScanLine(minX, maxX) :: scanLines

        | ScanLine (minX', maxX') :: rest when minX <= minX' && maxX >= maxX' ->
            // the first line in the list is completely inside the new line
            insertScanLine rest (ScanLine(minX, maxX))

        | ScanLine (minX', maxX') :: rest when minX <= minX' && maxX < maxX' ->
            // the first line in the list overlaps the new line
            insertScanLine rest (ScanLine(minX, maxX'))

        | ScanLine (minX', maxX') :: rest when minX > minX' && maxX >= maxX' ->
            // the first line in the list overlaps the new line
            insertScanLine rest (ScanLine(minX', maxX))

        | ScanLine (minX', maxX') :: rest when minX > minX' && maxX < maxX' ->
            // the first line in the list is completely inside the new line
            scanLines

    scanLines
    |> Seq.fold (fun acc line ->
        let line' = insertScanLine acc line
        // printfn "* Inserting line %O into %s => %s" line (printScanLines acc) (printScanLines line')
        line') []

let scanBeacons lineY (ProblemInput sensors) =
    sensors
    |> Seq.map (fun (SensorReport (_, beacon)) -> beacon)
    |> Set.ofSeq
    |> Seq.filter (fun beacon -> beacon.y = lineY)
    |> Seq.length

let findExcludedSquares lineY input =
    let scanLines = findAllScanLines lineY input
    let scanLines = mergeScanLines scanLines
    let beacons = scanBeacons lineY input

    scanLines
    |> Seq.sumBy (fun (ScanLine (minX, maxX)) -> maxX - minX + 1)
    |> (+) -beacons

let findFirstEmptyInScanLines maxScan scanLines =
    match scanLines with
    | [] -> Some 0
    | _ ->
        let rec loop lines =
            match lines with
            | [ScanLine (minX, maxX)] ->
                if maxX < maxScan then
                    Some (maxX + 1)
                else
                    None
            | ScanLine (minX, maxX) :: ScanLine (minX', _) :: _ when minX' > maxX + 1 ->
                Some (maxX + 1)
            | _ :: rest ->
                loop rest
        loop scanLines

let findFirstEmpty maxScan (ProblemInput sensors) =
    let rec loop lineY =
        if lineY > maxScan then
            None
        else
            let lines = findAllScanLines lineY (ProblemInput sensors) |> mergeScanLines
            match findFirstEmptyInScanLines maxScan lines with
            | Some empty -> Some (empty, lineY)
            | None -> loop (lineY + 1)

    loop 0

let execute lines =
    let input = parseInput lines
    let lineY = 2000000
    let result = findExcludedSquares lineY input
    printfn "On line %d there are %d excluded squares\n\n" lineY result

    let maxScan = 4000000
    match findFirstEmpty maxScan input with
    | None -> printfn "No empty squares"
    | Some (x, y) ->
        printfn "First empty square is at (%d, %d) = %d" x y (int64 x * 4000000L + int64 y)

Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

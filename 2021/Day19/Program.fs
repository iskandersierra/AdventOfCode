open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = { scanners: ScannerInput[] }

and ScannerInput = {
    index: int
    beacons: BeaconInput[]
}

and [<Struct>] BeaconInput = { x: int; y: int; z: int }

and OptimInput = { scanners: OptimScannerInput[] }

and OptimScannerInput = {
    index: int
    beacons: IDictionary<int, BeaconInput[]>
}

module BeaconInput =
    let ScannerRadius = 1000
    let add (b1: BeaconInput) (b2: BeaconInput) =
        { x = b1.x + b2.x; y = b1.y + b2.y; z = b1.z + b2.z }

    let sub (b1: BeaconInput) (b2: BeaconInput) =
        { x = b1.x - b2.x; y = b1.y - b2.y; z = b1.z - b2.z }

    let isInScannerRadius (b: BeaconInput) =
        abs b.x <= ScannerRadius && abs b.y <= ScannerRadius && abs b.z <= ScannerRadius

let scannerHeaderRegex = Regex(@"^[-]{3} scanner (?<index>\d+) [-]{3}$", RegexOptions.Compiled)
let beaconReadingRegex = Regex(@"^(?<x>\-?\d+),(?<y>\-?\d+),(?<z>\-?\d+)$", RegexOptions.Compiled)

let parseInput (text: string[]) =
    let scanners = ResizeArray()
    let beacons = ResizeArray()

    let createScanner index =
        if index <> scanners.Count then
            failwithf "Invalid scanner index: %d" index
        if beacons.Count = 0 then
            failwithf "No beacons for scanner: %d" index
        { ScannerInput.index = index
          beacons = beacons.ToArray() }
        |> scanners.Add
        beacons.Clear()

    for i = 0 to text.Length - 1 do
        let line = text.[i]
        match beaconReadingRegex.Match(line) with
        | m when m.Success ->
            let x = int m.Groups.["x"].Value
            let y = int m.Groups.["y"].Value
            let z = int m.Groups.["z"].Value
            beacons.Add { x = x; y = y; z = z }
        | _ ->
            match scannerHeaderRegex.Match(line) with
            | m when m.Success ->
                let index = int m.Groups.["index"].Value
                if index > 0 then
                    createScanner (index - 1)
            | _ ->
                match line with
                | "" -> ()
                | _ -> failwithf "Invalid line: %s at line %d" line (i + 1)
    createScanner scanners.Count
    { Input.scanners = scanners.ToArray() }

let rotations = [|
    fun b -> { x = b.x; y = b.y; z = b.z }
    fun b -> { x = b.x; y = b.z; z = -b.y }
    fun b -> { x = b.x; y = -b.y; z = -b.z }
    fun b -> { x = b.x; y = -b.z; z = b.y }

    fun b -> { x = b.y; y = b.x; z = b.z }
    fun b -> { x = b.y; y = b.z; z = -b.x }
    fun b -> { x = b.y; y = -b.x; z = -b.z }
    fun b -> { x = b.y; y = -b.z; z = b.x }

    fun b -> { x = b.z; y = b.x; z = b.y }
    fun b -> { x = b.z; y = b.y; z = -b.x }
    fun b -> { x = b.z; y = -b.x; z = -b.y }
    fun b -> { x = b.z; y = -b.y; z = b.x }

    fun b -> { x = -b.x; y = b.y; z = b.z }
    fun b -> { x = -b.x; y = b.z; z = -b.y }
    fun b -> { x = -b.x; y = -b.y; z = -b.z }
    fun b -> { x = -b.x; y = -b.z; z = b.y }

    fun b -> { x = -b.y; y = b.x; z = b.z }
    fun b -> { x = -b.y; y = b.z; z = -b.x }
    fun b -> { x = -b.y; y = -b.x; z = -b.z }
    fun b -> { x = -b.y; y = -b.z; z = b.x }

    fun b -> { x = -b.z; y = b.x; z = b.y }
    fun b -> { x = -b.z; y = b.y; z = -b.x }
    fun b -> { x = -b.z; y = -b.x; z = -b.y }
    fun b -> { x = -b.z; y = -b.y; z = b.x }
|]

let sortBeacons (beacons: BeaconInput[]) =
    let beacons = Array.copy beacons
    beacons
    |> Array.sortInPlaceWith (fun b1 b2 ->
        // First sort by x, then by y, then by z
        match b1.x.CompareTo b2.x with
        | 0 ->
            match b1.y.CompareTo b2.y with
            | 0 -> b1.z.CompareTo b2.z
            | c -> c
        | c -> c)
    beacons

let optimizeInput (input: Input) : OptimInput =
    let scanners =
        input.scanners
        |> Array.map (fun scanner ->
            let beacons = Dictionary()
            beacons.Add(0, scanner.beacons |> sortBeacons)
            { OptimScannerInput.index = scanner.index
              beacons = beacons }
        )
    { OptimInput.scanners = scanners }

let getScannerBeacons rotationIndex (scanner: OptimScannerInput) =
    match scanner.beacons.TryGetValue(rotationIndex) with
    | true, beacons -> beacons
    | false, _ ->
        let rotation = rotations.[rotationIndex]
        let rotatedBeacons =
            scanner.beacons.[0]
            |> Array.map rotation
            |> sortBeacons
        scanner.beacons.Add(rotationIndex, rotatedBeacons)
        rotatedBeacons

let findOverlap beacons1 beacons2 =
    // Check if there are at least 12 common beacons. They are sorted by x, y and z
    let memo = Dictionary()

    let rec loop index1 index2 (relativePosition: BeaconInput option) (pairs: (int * int) list) =
        let pairsLength = List.length pairs
        let beacons1Length = Array.length beacons1
        let beacons2Length = Array.length beacons2

        if 12 - pairsLength > min (beacons1Length - index1) (beacons2Length - index2) then
            None

        elif index1 >= beacons1Length && index2 >= beacons2Length then
            match relativePosition, pairs with
            | Some relativePosition, pairs ->
                // Maybe check that unselected beacons would get outside the 1000 radius cube of the other scanner
                // Assert that selected beacons are at the same position relative to given position
                Some (relativePosition, pairs)
            | _ ->
                None

        else
            let memoKey = (index1, index2, pairs)
            match memo.TryGetValue memoKey with
            | true, result -> result
            | false, _ ->
                // Either:
                // - beacons1[index1] is the same as beacons2[index2]
                // - beacons1[index1] is not in beacons2
                // - beacons2[index2] is not in beacons1
                let sameBeaconResult =
                    match relativePosition with
                    | None ->
                        let relativePosition = BeaconInput.sub beacons2.[index2] beacons1.[index1]
                        loop
                            (index1 + 1)
                            (index2 + 1)
                            (Some relativePosition)
                            ((index1, index2) :: pairs)
                    | Some relativePosition ->
                        if beacons2.[index2] = BeaconInput.add beacons1.[index1] relativePosition then
                            loop
                                (index1 + 1)
                                (index2 + 1)
                                (Some relativePosition)
                                ((index1, index2) :: pairs)
                        else
                            None

                let moveIndex1 =
                    let virtualPosition =
                        match relativePosition with
                        | None -> None
                        | Some pos -> Some (BeaconInput.add beacons1.[index1] pos)
                    match virtualPosition with
                    | Some virtualPosition when BeaconInput.isInScannerRadius virtualPosition ->
                        None
                    | _ ->
                        loop (index1 + 1) index2 relativePosition pairs

                let moveIndex2 =
                    let virtualPosition =
                        match relativePosition with
                        | None -> None
                        | Some pos -> Some (BeaconInput.sub beacons2.[index2] pos)
                    match virtualPosition with
                    | Some virtualPosition when BeaconInput.isInScannerRadius virtualPosition ->
                        None
                    | _ ->
                        loop index1 (index2 + 1) relativePosition pairs

                let bestResult =
                    seq {
                        yield! sameBeaconResult |> Option.toList
                        yield! moveIndex1 |> Option.toList
                        yield! moveIndex2 |> Option.toList
                    }
                    |> Seq.fold
                        (fun acc (relative, pairs) ->
                            match acc with
                            | Some (_, bestPairs) when List.length pairs > List.length bestPairs ->
                                Some (relative, pairs)
                            | _ -> acc)
                        None

                memo.Add(memoKey, bestResult)
                bestResult

    loop 0 0 None []

let step1 (input: OptimInput) =
    for rotation = 0 to rotations.Length - 1 do
        let overlap =
            findOverlap
                (getScannerBeacons 0 input.scanners.[0])
                (getScannerBeacons rotation input.scanners.[1])
        printfn "- %d: %A" rotation overlap
    1

let step2 (input: OptimInput) =
    2

let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let input = optimizeInput input
    // printfn "OptimInput:\n%A" input

    let n = 1
    let time = Time.measureColdN

    (fun () -> step1 input)
    |> time n
    |> printfn "***** STEP1:\n%O"

    // (fun () -> step2 input)
    // |> time n
    // |> printfn "***** STEP2:\n%O"


main ()

open System
open System.IO

#nowarn "0025"
open System.Text.RegularExpressions

type Rearrangement = Rearrangement of count: int * fromStack: int * toStack: int
type PuzzleInput = PuzzleInput of stacks: Map<int, char list> * rearrangements: Rearrangement list

let RearrangementPattern =
    Regex(
        @"^move (?<count>\d+) from (?<source>\d+) to (?<dest>\d+)$",
        RegexOptions.Compiled ||| RegexOptions.IgnoreCase
    )

let ArrangementPattern =
    Regex(
        @"^((\s(?<crate>\s)\s)|(\[(?<crate>\w)\]))(\s((\s(?<crate>\s)\s)|(\[(?<crate>\w)\])))*$",
        RegexOptions.Compiled ||| RegexOptions.IgnoreCase
    )

let matchRegex (regex: Regex) (input: string) =
    let m = regex.Match(input)
    if m.Success then Some m else None

let (|RearrangementLine|_|) line =
    matchRegex RearrangementPattern line
    |> Option.map (fun m ->
        let count = int m.Groups.["count"].Value
        let fromStack = int m.Groups.["source"].Value
        let toStack = int m.Groups.["dest"].Value
        Rearrangement(count, fromStack, toStack))

let (|ArrangementLine|_|) line =
    matchRegex ArrangementPattern line
    |> Option.map (fun m ->
        m.Groups.["crate"].Captures
        |> Seq.map (fun c -> c.Value.[0])
        |> List.ofSeq)

let makeStacks lines =
    let rec loop lines index stackMap =
        match lines with
        | [] -> stackMap
        | [] :: rest -> loop rest 1 stackMap
        | (' ' :: stack) :: rest -> loop (stack :: rest) (index + 1) stackMap
        | (crate :: stack) :: rest ->
            let crates =
                match stackMap |> Map.tryFind index with
                | None -> [ crate ]
                | Some crates -> crate :: crates

            loop (stack :: rest) (index + 1) (stackMap |> Map.add index crates)

    loop lines 1 Map.empty

let parseInput lines =
    let rec readLines arrangementLines rearrangements lines =
        match lines with
        | [] -> PuzzleInput(makeStacks arrangementLines, List.rev rearrangements)
        | RearrangementLine rearrangement :: rest -> readLines arrangementLines (rearrangement :: rearrangements) rest
        | ArrangementLine (crates) :: rest -> readLines (crates :: arrangementLines) rearrangements rest
        | _ :: rest -> readLines arrangementLines rearrangements rest

    lines |> Seq.toList |> readLines [] []

let rec crateMover9000 count source dest =
    match count, source, dest with
    | 0, _, _ -> source, dest
    | count, (item :: source), dest when count > 0 -> crateMover9000 (count - 1) source (item :: dest)

let rec crateMover9001 count source dest =
    let items = source |> List.take count
    let source = source |> List.skip count
    let dest = items @ dest
    source, dest

let runProcedure itemMover (PuzzleInput (stacks, rearrangements)) =
    let rec loop stacks rearrangements =
        match rearrangements with
        | [] -> stacks
        | Rearrangement (count, fromStack, toStack) :: rest ->
            let source = stacks |> Map.find fromStack
            let dest = stacks |> Map.find toStack
            // printfn "Moving %d from %d to %d (%A ==> %A)" count fromStack toStack source dest
            let source, dest = itemMover count source dest
            // printfn "    %A ==> %A" source dest
            let stacks =
                stacks
                |> Map.add fromStack source
                |> Map.add toStack dest

            loop stacks rest

    loop stacks rearrangements

let getTopCrates (stacks: Map<int, char list>) =
    stacks
    |> Map.values
    |> Seq.map List.head
    |> Seq.map string
    |> String.concat ""

let part1 lines =
    lines
    |> parseInput
    |> runProcedure crateMover9000
    |> getTopCrates

let part2 lines =
    lines
    |> parseInput
    |> runProcedure crateMover9001
    |> getTopCrates

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllLines fileName
|> if part = "1" then part1 else part2
|> printfn "%A"

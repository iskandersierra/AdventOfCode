open System
open System.IO

// A=Rock, B=Paper, C=Scissors
// Part1: X=Rock, Y=Paper, Z=Scissors
// Part2: X=Loose, Y=Draw, Z=Win

let computeChoicePart1 =
    function
    | 'X' -> 'A'
    | 'Y' -> 'B'
    | 'Z' -> 'C'
    | c -> failwith "Invalid choice"

let computeChoicePart2 opponent =
    function
    | 'X' ->
        match opponent with
        | 'A' -> 'C'
        | 'B' -> 'A'
        | 'C' -> 'B'
        | _ -> failwith "Invalid opponent"
    | 'Y' -> opponent
    | 'Z' ->
        match opponent with
        | 'A' -> 'B'
        | 'B' -> 'C'
        | 'C' -> 'A'
        | _ -> failwith "Invalid opponent"
    | c -> failwith "Invalid choice"

let valueChoice =
    function
    | 'A' -> 1
    | 'B' -> 2
    | 'C' -> 3
    | _ -> failwith "Invalid choice"

let valueRound opponent choice =
    match (opponent, choice) with
    | 'A', 'A'
    | 'B', 'B'
    | 'C', 'C' -> 3
    | 'A', 'B'
    | 'B', 'C'
    | 'C', 'A' -> 6
    | 'A', 'C'
    | 'B', 'A'
    | 'C', 'B' -> 0
    | _ -> failwith "Invalid choice"

let part1 (lines: string seq) =
    lines
    |> Seq.map (fun line -> line.[0], computeChoicePart1 line.[2])
    |> Seq.map (fun (opponent, choice) -> valueRound opponent choice + valueChoice choice)
    |> Seq.sum

let part2 (lines: string seq) =
    lines
    |> Seq.map (fun line -> line.[0], computeChoicePart2 line.[0] line.[2])
    |> Seq.map (fun (opponent, choice) -> valueRound opponent choice + valueChoice choice)
    |> Seq.sum

let argv = Environment.GetCommandLineArgs().[2..3]
let fileName, part = argv.[0], argv.[1]
let lines = File.ReadAllLines fileName

match part with
| "1" -> part1 lines
| "2" -> part2 lines
| _ -> failwith "Invalid part"
|> printfn "%d"

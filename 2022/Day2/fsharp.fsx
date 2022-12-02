open System
open System.IO
#nowarn "0025"

// A=Rock, B=Paper, C=Scissors
// Part1: X=Rock, Y=Paper, Z=Scissors
// Part2: X=Loose, Y=Draw, Z=Win

let computeChoicePart1 = function
    | 'X' -> 'A' | 'Y' -> 'B' | 'Z' -> 'C'
    | c -> failwith "Invalid choice"

let computeChoicePart2 opponent = function
    | 'X' ->
        match opponent with
        | 'A' -> 'C' | 'B' -> 'A' | 'C' -> 'B'
        | _ -> failwith "Invalid opponent"
    | 'Y' -> opponent
    | 'Z' ->
        match opponent with
        | 'A' -> 'B' | 'B' -> 'C' | 'C' -> 'A'
        | _ -> failwith "Invalid opponent"
    | c -> failwith "Invalid choice"

let valueChoice = function
    | 'A' -> 1 | 'B' -> 2 | 'C' -> 3
    | _ -> failwith "Invalid choice"

let valueRound opponent choice =
    match (opponent, choice) with
    | 'A', 'C' | 'B', 'A' | 'C', 'B' -> 0
    | 'A', 'A' | 'B', 'B' | 'C', 'C' -> 3
    | 'A', 'B' | 'B', 'C' | 'C', 'A' -> 6
    | _ -> failwith "Invalid choice"

let part1 (line: string) = line.[0], computeChoicePart1 line.[2]
let part2 (line: string) = line.[0], computeChoicePart2 line.[0] line.[2]
let [|fileName; part|] = Environment.GetCommandLineArgs().[2..3]
let lineProcessor = if part = "1" then part1 else part2

File.ReadAllLines fileName
|> Seq.map lineProcessor
|> Seq.map (fun (opponent, choice) -> valueRound opponent choice + valueChoice choice)
|> Seq.sum
|> printfn "%d"

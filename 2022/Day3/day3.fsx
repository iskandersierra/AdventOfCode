open System
open System.IO

#nowarn "0025"

let priorities =
    [ for i in 0 .. 25 -> char (i + 97) ] @ [ for i in 0 .. 25 -> char (i + 65) ]
    |> Seq.mapi (fun i item -> item, i + 1)
    |> Map.ofSeq

let createRucksack (line: string) =
    line.Substring(0, String.length line / 2) |> Set,
    line.Substring(String.length line / 2) |> Set

let evaluateRucksack a b =
    Set.intersect a b
    |> Set.map (fun item -> priorities.[item])
    |> Seq.sum

let findBadge rucksacks =
    rucksacks
    |> Seq.map (fun (a, b) -> Set.union a b)
    |> Set.intersectMany
    |> Seq.exactlyOne

let part1 lines =
    lines
    |> Seq.map createRucksack
    |> Seq.map (fun (a, b) -> evaluateRucksack a b)
    |> Seq.sum

let part2 lines =
    lines
    |> Seq.map createRucksack
    |> Seq.chunkBySize 3
    |> Seq.map findBadge
    |> Seq.map (fun badge -> priorities.[badge])
    |> Seq.sum

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllLines fileName
|> if part = "1" then part1 else part2
|> printfn "%A"

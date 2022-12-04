open System
open System.IO

#nowarn "0025"
open System.Text.RegularExpressions

type Assignment = int * int
type AssignmentPair = Assignment * Assignment

let PairRegex = Regex(@"^((?<from1>\d+)\-(?<to1>\d+)),((?<from2>\d+)\-(?<to2>\d+))$", RegexOptions.Compiled)
let parseAssignmentPair line : AssignmentPair =
    let m = PairRegex.Match(line)
    let from1 = int m.Groups.["from1"].Value
    let to1 = int m.Groups.["to1"].Value
    let from2 = int m.Groups.["from2"].Value
    let to2 = int m.Groups.["to2"].Value
    ((from1, to1), (from2, to2))

let isContainedIn (from1, to1) (from2, to2) = from1 >= from2 && to1 <= to2
let isContainedBackOrFront a b = isContainedIn a b || isContainedIn b a
let overlapsWith (from1, to1) (from2, to2) = not (to1 < from2 || to2 < from1)

let part1 lines =
    lines
    |> Seq.map parseAssignmentPair
    |> Seq.filter (fun (a, b) -> isContainedBackOrFront a b)
    |> Seq.length

let part2 lines =
    lines
    |> Seq.map parseAssignmentPair
    |> Seq.filter (fun (a, b) -> overlapsWith a b)
    |> Seq.length

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllLines fileName
|> if part = "1" then part1 else part2
|> printfn "%A"

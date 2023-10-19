open System.Text.RegularExpressions

type Instruction =
    | Forward of int
    | Down of int
    | Up of int

let lineRegex = Regex(@"^(?<t>forward|down|up)\s(?<c>\d+)$")

let parseLine line =
    let m = lineRegex.Match(line)
    if m.Success then
        let t = m.Groups.["t"].Value
        let c = int m.Groups.["c"].Value
        match t with
        | "forward" -> Forward c
        | "down" -> Down c
        | "up" -> Up c
        | _ -> failwithf "Unknown instruction: %s" t
    else
        failwithf "Invalid line: %s" line

let step1 instructions =
    let mutable horz = 0
    let mutable depth = 0
    for i = 0 to Array.length instructions - 1 do
        match instructions.[i] with
        | Forward c -> horz <- horz + c
        | Down c -> depth <- depth + c
        | Up c -> depth <- depth - c
    horz * depth

let step2 instructions =
    let mutable aim = 0
    let mutable horz = 0
    let mutable depth = 0
    for i = 0 to Array.length instructions - 1 do
        match instructions.[i] with
        | Forward c ->
            horz <- horz + c
            depth <- depth + aim * c
        | Down c -> aim <- aim + c
        | Up c -> aim <- aim - c
    horz * depth


let main() =
    let input =
        Input.parseInputLines()
        |> Array.map parseLine

    let n = Array.length input

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1: %d" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2: %d" result2

main()

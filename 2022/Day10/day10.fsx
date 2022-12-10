open System
open System.IO
open System.Text
open System.Text.RegularExpressions

#nowarn "0025"

type Instruction =
    | AddX of int
    | Noop

let (|ParseInstruction|_|) line =
    if line = "noop" then
        Some Noop
    else
        let m =
            Regex.Match(line, @"^addx\s(?<value>\-?\d+)$")

        if m.Success then
            Some(AddX(int m.Groups.["value"].Value))
        else
            None

let asInstructions lines =
    lines
    |> Seq.collect (fun line ->
        match line with
        | ParseInstruction instruction -> seq { instruction }
        | "" -> Seq.empty
        | _ -> failwithf "Invalid instruction: %s" line)

let runInstructions instructions =
    seq {
        let mutable cycle = 0
        let mutable register = 1
        yield cycle, register

        for instruction in instructions do
            match instruction with
            | Noop ->
                cycle <- cycle + 1
                yield cycle, register
            | AddX x ->
                yield cycle + 1, register
                cycle <- cycle + 2
                register <- register + x
                yield cycle, register
    }

let part1 lines =
    lines
    |> asInstructions
    |> runInstructions
    |> Seq.filter (fun (cycle, _) -> (cycle - 19) % 40 = 0)
    |> Seq.map (fun (cycle, register) -> (cycle + 1) * register)
    |> Seq.sum
    |> string

let part2 lines =
    let cycles =
        lines |> asInstructions |> runInstructions

    let sb = new StringBuilder()

    for (cycle, register) in cycles do
        let pixel =
            if abs (register - cycle % 40) <= 1 then
                '#'
            else
                ' '

        sb.Append(pixel) |> ignore

        if (cycle + 1) % 40 = 0 then
            sb.AppendLine() |> ignore

    sb.ToString()


let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllLines fileName
|> if part = "1" then part1 else part2
|> printfn "%s"

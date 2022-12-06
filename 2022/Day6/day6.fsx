open System
open System.IO

#nowarn "0025"

let findMarker size line =
    let rec loop index current line =
        match line with
        | [] -> -1
        | ch :: line ->
            let current = current @ [ ch ]

            let current =
                if List.length current > size then
                    current |> List.skip 1
                else
                    current

            if current |> List.distinct |> List.length = size then
                index
            else
                loop (index + 1) current line

    loop 1 [] (line |> Seq.toList)

let part1 line = findMarker 4 line

let part2 line = findMarker 14 line

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

if fileName.EndsWith ".txt" then
    File.ReadAllText fileName
else
    fileName
|> fun s -> s.Trim()
|> if part = "1" then part1 else part2
|> printfn "%A"

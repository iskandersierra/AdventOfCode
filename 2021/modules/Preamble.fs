[<AutoOpen>]
module Preamble

open System
open System.IO
open System.Diagnostics

module Seq =
    let tee f = Seq.map (fun a -> f a; a)

module Array =
    let windows windowSize data =
        seq {
            for i = 0 to Array.length data - windowSize do
                yield ArraySegment(data, i, windowSize)
        }

module String =
    let splitByChar (ch: char) (source: string) =
        source.Split([|ch|], StringSplitOptions.RemoveEmptyEntries ||| StringSplitOptions.TrimEntries)


module Time =
    let sprintElapsed (time: TimeSpan) =
        let ms = time.TotalMilliseconds
        if ms >= 1.0 then
            sprintf "%0.3f ms" ms
        else
            let µs = ms * 1000.0
            if µs >= 1.0 then
                sprintf "%0.3f µs" µs
            else
                let ns = µs * 1000.0
                if ns >= 1.0 then
                    sprintf "%0.3f ns" ns
                else
                    sprintf "%0.3f ps" (ns * 1000.0)

    let sprintElapsedN (n: int) (time: TimeSpan) =
        let perInput = time.Divide(float n)
        let total = sprintElapsed time
        let perInput = sprintElapsed perInput
        $"{total} ({perInput}/input)"

    let measureCold f =
        let sw = Stopwatch.StartNew()
        let result = f()
        sw.Stop()
        sprintElapsed sw.Elapsed |> printfn "Elapsed: %s"
        result

    let measureColdN (n: int) f =
        let sw = Stopwatch.StartNew()
        let result = f()
        sw.Stop()
        sprintElapsedN n sw.Elapsed |> printfn "Elapsed: %s"
        result

    let measure f =
        f() |> ignore
        let sw = Stopwatch.StartNew()
        let result = f()
        sw.Stop()
        sprintElapsed sw.Elapsed |> printfn "Elapsed: %s"
        result

    let measureN (n: int) f =
        f() |> ignore
        let sw = Stopwatch.StartNew()
        let result = f()
        sw.Stop()
        sprintElapsedN n sw.Elapsed |> printfn "Elapsed: %s"
        result

module Input =
    let parseInputLines () =
        match Environment.GetCommandLineArgs() with
        | [|_; filename|] ->
            File.ReadAllLines(filename)

        | _ -> failwith "Please specify input file name"

    let parseInputText () =
        match Environment.GetCommandLineArgs() with
        | [|_; filename|] ->
            File.ReadAllText(filename)

        | _ -> failwith "Please specify input file name"

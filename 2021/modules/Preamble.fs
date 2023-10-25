[<AutoOpen>]
module Preamble

open System
open System.IO
open System.Diagnostics

module Seq =
    let tee f = Seq.map (fun a -> f a; a)

    let takeAtMost (n: int) (source: 'a seq) =
        seq {
            use enumerator = source.GetEnumerator()
            let mutable i = 0
            while i < n && enumerator.MoveNext() do
                yield enumerator.Current
                i <- i + 1
        }

module Array =
    let windows windowSize data =
        seq {
            for i = 0 to Array.length data - windowSize do
                yield ArraySegment(data, i, windowSize)
        }

    let iterateCombinations f combinationSize elementCount =
        let rec loop (combination: int[]) (used: bool[]) index =
            if index >= elementCount then
                f combination
            else
                let rec innerLoop element =
                    if element >= elementCount then
                        ValueNone
                    else
                        if not used.[element] then
                            combination.[index] <- element
                            used.[element] <- true
                            match loop combination used (index + 1) with
                            | ValueSome result -> ValueSome result
                            | ValueNone ->
                                used.[element] <- false
                                innerLoop (element + 1)
                        else
                            innerLoop (element + 1)

                innerLoop 0

        let combination = Array.zeroCreate combinationSize
        let used = Array.zeroCreate elementCount
        loop combination used 0

module Array2D =
    let clearWith value source =
        Array2D.iteri (fun i j _ -> Array2D.set source i j value) source

    let countBy f source =
        let l1 = Array2D.length1 source - 1
        let l2 = Array2D.length2 source - 1
        let mutable result = 0
        for i = 0 to l1 do
            for j = 0 to l2 do
                if f (Array2D.get source i j) then
                    result <- result + 1
        result

    let countByij f source =
        let l1 = Array2D.length1 source - 1
        let l2 = Array2D.length2 source - 1
        let mutable result = 0
        for i = 0 to l1 do
            for j = 0 to l2 do
                if f i j (Array2D.get source i j) then
                    result <- result + 1
        result

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
                sprintf "%0.3f ns" ns

    let sprintElapsedN (n: int) (time: TimeSpan) =
        let perInput = if n > 0 then time.Divide(float n) else time
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

module OptionalBuilder =
    type OptionalBuilder() =
        member inline __.Bind (x, f) = Option.bind f x
        member inline __.Return x = Some x
        member inline __.ReturnFrom x = x

let optional = OptionalBuilder.OptionalBuilder()

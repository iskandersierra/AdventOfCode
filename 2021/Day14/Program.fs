open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input =
    { template: string
      insertions: Insertion [] }

and [<Struct>] Insertion = { snippet: string; toInsert: char }

type OptimInput1 =
    { template: OptimResult1
      insertions: IDictionary<string, char> }

    static member FromInput(input: Input) =
        let insertions =
            input.insertions
            |> Seq.map (fun i -> (i.snippet, i.toInsert))
            |> dict

        { OptimInput1.template = OptimResult1.FromTemplate input.template
          insertions = insertions }

and Output1 =
    { result: int
      results: OptimResult1 [] }
    override this.ToString() =
        let sb = new StringBuilder()

        this.results
        |> Array.iteri (fun i result ->
            sb.Append($"- Step {i} [{result.result}]: ")
            |> ignore

            if result.template.Length <= 80 then
                sb.Append(result.template) |> ignore
            else
                sb
                    .Append(result.template.Substring(0, 65))
                    .Append("...")
                    .Append(result.template.Substring(result.template.Length - 12))
                |> ignore

            sb.AppendLine($@" [{result.template.Length}]")
            |> ignore)

        sb.AppendLine($"Result: {this.result}") |> ignore

        sb.ToString()

and CharCounts = IDictionary<char, int>

and OptimResult1 =
    { template: string
      counts: CharCounts
      result: int }
    static member FromTemplate(template: string) =
        let counts = template |> Seq.countBy id |> dict

        let mostFrequentChar =
            counts
            |> Seq.maxBy (fun kvp -> kvp.Value)
            |> fun kvp -> kvp.Value

        let leastFrequentChar =
            counts
            |> Seq.minBy (fun kvp -> kvp.Value)
            |> fun kvp -> kvp.Value

        let result = mostFrequentChar - leastFrequentChar

        { template = template
          counts = counts
          result = result }

and Output2 =
    { result: OptimResult2 }
    override this.ToString() =
        let sb = new StringBuilder()

        this.result.counts
        |> Seq.sortByDescending (fun kvp -> kvp.Value)
        |> Seq.iter (fun pair ->
            sb.AppendLine($"- CHAR {pair.Key} has {pair.Value}")
            |> ignore)

        sb.AppendLine($"Result: {this.result.result} from a total length of {this.result.templateLength}")
        |> ignore

        sb.ToString()

and CharCounts64 = IDictionary<char, uint64>

and OptimResult2 =
    { templateLength: uint64
      counts: IDictionary<char, uint64>
      result: uint64 }

let insertionRegex =
    Regex(@"^(?<from>[A-Z]{2}) \-> (?<to>[A-Z])$", RegexOptions.Compiled)

let templateRegex =
    Regex(@"^(?<template>[A-Z]+)$", RegexOptions.Compiled)

let parseInput (text: string []) : Input =
    let templates = ResizeArray()
    let insertions = ResizeArray()

    for i = 0 to text.Length - 1 do
        let line = text.[i]

        match templateRegex.Match line with
        | m when m.Success && templates.Count = 0 ->
            let template = m.Groups.["template"].Value
            templates.Add(template)

        | m when m.Success -> failwithf "Found a second template: %s" line

        | _ ->
            match insertionRegex.Match line with
            | m when m.Success ->
                let from = m.Groups.["from"].Value
                let toInsert = m.Groups.["to"].Value.[0]
                insertions.Add({ snippet = from; toInsert = toInsert })

            | _ ->
                match line with
                | "" -> ()
                | _ ->
                    failwithf
                        "Invalid input line: %s. Expecting a template or an insertion.\nRegex: %O\nRegex: %O"
                        line
                        insertionRegex
                        templateRegex

    if templates.Count <> 1 then
        failwithf "Expected exactly one template, but found %d" templates.Count

    { template = templates.[0]
      insertions = insertions.ToArray() }

let polymerize (insertions: IDictionary<string, char>) (template: OptimResult1) =
    let template = template.template
    let lastIndex = template.Length - 2
    let sb = new StringBuilder()

    for i = 0 to lastIndex do
        let pair = template.Substring(i, 2)

        match insertions.TryGetValue(pair) with
        | true, toInsert -> sb.Append(pair.[0]).Append(toInsert) |> ignore
        | false, _ -> sb.Append(pair.[0]) |> ignore

    sb.Append(template.[lastIndex + 1]) |> ignore
    sb.ToString() |> OptimResult1.FromTemplate

let solveNaive depth (input: OptimInput1) : Output1 =
    let mutable current = input.template
    let results = ResizeArray()
    results.Add(current)

    for _ = 1 to depth do
        let result = polymerize input.insertions current
        results.Add(result)
        current <- result

    let result = results.[results.Count - 1]

    { result = result.result
      results = results.ToArray() }

[<Struct>]
type SolveKey = { depth: int; pair: string }

[<Struct>]
type SolveValue =
    { counts: CharCounts64
      length: uint64 }

let addCounts (counts1: CharCounts64) (counts2: CharCounts64) : CharCounts64 =
    let counts = Dictionary(counts1)

    counts2
    |> Seq.iter (fun pair ->
        match counts.TryGetValue(pair.Key) with
        | true, value -> counts.[pair.Key] <- value + pair.Value
        | false, _ -> counts.Add(pair.Key, pair.Value))

    counts

let getPairCounts (pair: string) : CharCounts64 =
    let counts = Dictionary<char, uint64>()

    for i = 0 to pair.Length - 2 do
        let c = pair.[i]

        match counts.TryGetValue(c) with
        | true, value -> counts.[c] <- value + 1UL
        | false, _ -> counts.Add(c, 1UL)

    counts

let solve depth (input: OptimInput1) : Output2 =
    let memory = Dictionary<SolveKey, SolveValue>()

    let rec loop (key: SolveKey) =
        match memory.TryGetValue(key) with
        | true, result -> result
        | false, _ ->
            let newValue =
                if key.depth <= 0 then
                    let counts = getPairCounts key.pair
                    let length = uint64 key.pair.Length - 1UL
                    { counts = counts; length = length }

                else
                    match input.insertions.TryGetValue(key.pair) with
                    | true, toInsert ->
                        let pair1 = $"{key.pair.[0]}{toInsert}"
                        let pair2 = $"{toInsert}{key.pair.[1]}"

                        let value1 =
                            loop { depth = key.depth - 1; pair = pair1 }

                        let value2 =
                            loop { depth = key.depth - 1; pair = pair2 }

                        let counts = addCounts value1.counts value2.counts
                        let length = value1.length + value2.length
                        { counts = counts; length = length }

                    | false, _ ->
                        let counts = getPairCounts key.pair
                        let length = uint64 key.pair.Length // 2
                        { counts = counts; length = length }

            memory.Add(key, newValue)
            newValue

    let mutable current = { counts = dict []; length = 0UL }
    let template = input.template.template

    for i = 0 to template.Length - 2 do
        let pair = template.Substring(i, 2)
        let value = loop { depth = depth; pair = pair }
        let currentCounts = addCounts current.counts value.counts
        let currentLength = current.length + value.length

        current <-
            { counts = currentCounts
              length = currentLength }

    current <- {
        counts = addCounts current.counts (dict [ template.[template.Length - 1], 1UL ])
        length = current.length + 1UL }

    let mostCommonCharCount =
        current.counts
        |> Seq.maxBy (fun kvp -> kvp.Value)
        |> fun kvp -> kvp.Value

    let leastCommonCharCount =
        current.counts
        |> Seq.minBy (fun kvp -> kvp.Value)
        |> fun kvp -> kvp.Value

    let result =
        mostCommonCharCount - leastCommonCharCount

    { Output2.result =
        { templateLength = current.length
          counts = current.counts
          result = result } }

let step1 (input: OptimInput1) = solve 10 input

let step2 (input: OptimInput1) = solve 40 input

let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let optimInput = OptimInput1.FromInput input
    // printfn "Optimized Input: %A" optimInput

    let n =
        optimInput.template.template.Length
        * optimInput.insertions.Count
        * 10

    let result1 = Time.measureN n (fun () -> step1 optimInput)

    printfn "***** STEP1:\n%O" result1

    let n = n * 4

    let result2 = Time.measureN n (fun () -> step2 optimInput)

    printfn "***** STEP2:\n%O" result2

main ()

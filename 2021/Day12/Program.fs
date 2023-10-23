open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = InputEdge []

and InputEdge = { fromCave: string; toCave: string }

type OptimInput1 =
    { input: Input
      nodeIndices: IDictionary<string, int>
      nodeNames: string []
      canRepeat: bool []
      outboundEdges: int [] [] }

type Output1 =
    { pathCount: int
      paths: CavePath [] }
    override this.ToString() =
        let sb = new StringBuilder()

        // sb.AppendLine($"Paths [this.pathCount]: ") |> ignore
        // this.paths
        // |> Seq.map (fun path -> path.ToString())
        // |> Seq.sort
        // |> Seq.iter (fun path -> sb.AppendLine($"  {path}") |> ignore)

        sb.AppendLine($"Path count: {this.pathCount}")
        |> ignore

        sb.ToString()

and CavePath =
    { caves: string [] }
    override this.ToString() = String.concat "," this.caves

let edgeRegex =
    let node = @"[a-z]+|[A-Z]+"
    Regex($@"^(?<from>{node})\-(?<to>{node})$", RegexOptions.Compiled)

let parseInput (text: string []) : Input =
    text
    |> Array.map (fun line ->
        let m = edgeRegex.Match line

        if m.Success then
            let fromCave = m.Groups.["from"].Value
            let toCave = m.Groups.["to"].Value
            { fromCave = fromCave; toCave = toCave }
        else

            failwithf "Expected edge in format 'from-to' but got '%s'.\nRegex: %O" line edgeRegex)

let StartIndex = 0
let EndIndex = 1

let canRepeatRegex =
    let node = @"[A-Z]+"
    Regex($@"^{node}$", RegexOptions.Compiled)

let optimizeInput (input: Input) =
    let nodeIndices =
        let d = Dictionary<string, int>()
        d.Add("start", StartIndex)
        d.Add("end", EndIndex)
        let mutable nextIndex = EndIndex + 1

        input
        |> Seq.collect (fun edge -> [ edge.fromCave; edge.toCave ])
        |> Seq.iter (fun name ->
            if not (d.ContainsKey name) then
                d.Add(name, nextIndex)
                nextIndex <- nextIndex + 1)

        d :> IDictionary<string, int>

    let nodeNames =
        nodeIndices
        |> Seq.sortBy (fun kvp -> kvp.Value)
        |> Seq.map (fun kvp -> kvp.Key)
        |> Seq.toArray

    let canRepeat =
        nodeNames
        |> Array.map (fun name -> canRepeatRegex.IsMatch name)

    let outboundEdges =
        let arr = Array.zeroCreate nodeNames.Length

        for i = 0 to nodeNames.Length - 1 do
            let targets =
                let toEdges =
                    input
                    |> Array.filter (fun edge -> edge.toCave = nodeNames.[i])
                    |> Array.map (fun edge -> nodeIndices.[edge.fromCave])

                let fromEdges =
                    input
                    |> Array.filter (fun edge -> edge.fromCave = nodeNames.[i])
                    |> Array.map (fun edge -> nodeIndices.[edge.toCave])

                Array.concat [ toEdges; fromEdges ]
                |> Array.sort
                |> Array.distinct

            arr.[i] <- targets

        arr

    { input = input
      nodeIndices = nodeIndices
      nodeNames = nodeNames
      canRepeat = canRepeat
      outboundEdges = outboundEdges }

let step1 (input: OptimInput1) =
    let paths = ResizeArray()

    let rec loop revPath current visited =
        if current = EndIndex then
            let caves =
                List.rev revPath
                |> List.map (fun i -> input.nodeNames.[i])
                |> Array.ofList

            paths.Add({ caves = caves })
        else
            let targets = input.outboundEdges.[current]

            for i = 0 to targets.Length - 1 do
                let target = targets.[i]
                let canRepeat = input.canRepeat.[target]

                if canRepeat || Array.get visited target = false then
                    visited.[target] <- true
                    loop (target :: revPath) target visited
                    visited.[target] <- false

    let visited = Array.zeroCreate input.nodeNames.Length
    visited.[StartIndex] <- true
    loop [ 0 ] 0 visited

    { pathCount = paths.Count
      paths = paths.ToArray() }

let step2 (input: OptimInput1) =
    let paths = ResizeArray()

    let rec loop revPath current visited doubleVisited =
        if current = EndIndex then
            let caves =
                List.rev revPath
                |> List.map (fun i -> input.nodeNames.[i])
                |> Array.ofList

            paths.Add({ caves = caves })
        else
            let targets = input.outboundEdges.[current]

            for i = 0 to targets.Length - 1 do
                let target = targets.[i]
                let canRepeat = input.canRepeat.[target]
                let isTargetVisited = Array.get visited target

                if canRepeat then
                    loop (target :: revPath) target visited doubleVisited
                elif not isTargetVisited then
                    visited.[target] <- true
                    loop (target :: revPath) target visited doubleVisited
                    visited.[target] <- false
                elif not doubleVisited && target > EndIndex then
                    loop (target :: revPath) target visited true

    let visited = Array.zeroCreate input.nodeNames.Length
    visited.[StartIndex] <- true
    loop [ 0 ] 0 visited false

    { pathCount = paths.Count
      paths = paths.ToArray() }


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input: %A" input
    let optimInput = optimizeInput input
    // printfn "Optimized Input: %A" optimInput

    let n = input.Length

    let result1 =
        Time.measureN n (fun () -> step1 optimInput)

    printfn "***** STEP1:\n%O" result1

    let result2 =
        Time.measureN n (fun () -> step2 optimInput)

    printfn "***** STEP2:\n%O" result2

main ()

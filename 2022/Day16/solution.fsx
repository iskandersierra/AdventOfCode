#r "nuget: FParsec, 1.1.1"
#load "../../modules/Graphs.fsx"

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open FParsec
open Graphs

#nowarn "0025"

[<Measure>]
type minute

[<Measure>]
type presure

[<Measure>]
type flowRate = presure / minute

let RemainingTime = 30<minute>
let StartingValve = "AA"
let TimeToOpenValve = 1<minute>
let TimeToTraverseTunnel = 1<minute>

type Valve = string

type ValveInput =
    | ValveInput of name: Valve * nextValves: Valve list * flowRate: int<flowRate>
    override this.ToString() =
        let (ValveInput (name, nextValves, flowRate)) = this
        sprintf "Valve %s has flow rate %2d and connects to %s" name flowRate (String.concat ", " nextValves)

    static member Create name flowRate nextValves = ValveInput(name, nextValves, flowRate)

type ProblemInput =
    | ProblemInput of lines: ValveInput list
    override this.ToString() =
        let (ProblemInput lines) = this

        lines
        |> List.map (sprintf "%O")
        |> String.concat "\n"

type ValveInfo =
    { name: Valve
      flowRate: int<flowRate>
      nextValves: Valve list }

type Problem = Map<Valve, ValveInfo>

let parseInput lines =
    let parseValveInput: Parser<ValveInput, unit> =
        let parseName = many1Chars letter
        let parseFlowRate = pint32 |>> fun x -> x * 1<flowRate>
        let parseTunnels = sepBy1 parseName (pstring ", ")

        let parseLeadText =
            choice [ pstring "; tunnels lead to valves "
                     pstring "; tunnel leads to valve " ]

        pipe3
            (pstring "Valve " >>. parseName)
            (pstring " has flow rate=" >>. parseFlowRate)
            (parseLeadText >>. parseTunnels)
            ValveInput.Create

    let parseProblemInput: Parser<ProblemInput, unit> =
        (sepBy1 parseValveInput newline) .>> eof
        |>> ProblemInput

    match run parseProblemInput lines with
    | Success (result, _, _) -> result
    | Failure (error, _, _) -> failwith error

let toProblem (ProblemInput lines) : Problem =
    lines
    |> List.map (fun (ValveInput (name, nextValves, flowRate)) ->
        name,
        { name = name
          flowRate = flowRate
          nextValves = nextValves })
    |> Map.ofList

type NodeLabel = Valve

type NodeState =
    { remainingTime: int<minute>
      openValves: Map<Valve, int<minute>> }

let isOpen valve state =
    state.openValves |> Map.containsKey valve

let totalPressure (problem: Problem) state =
    state.openValves
    |> Map.toList
    |> List.map (fun (valve, remaining) -> problem.[valve].flowRate * remaining)
    |> List.sum

let currentPressure (problem: Problem) state =
    state.openValves
    |> Map.toList
    |> List.map (fun (valve, _) -> problem.[valve].flowRate)
    |> List.sum

let printState problem valve state =
    let sb = StringBuilder()

    let currentMinute =
        RemainingTime - state.remainingTime + 1<minute>

    let status =
        if isOpen valve state then
            "open"
        else
            "closed"

    let totalPresure = totalPressure problem state

    sb.AppendLine(sprintf "== Minute %d at %s valve %s [%d] ==" currentMinute status valve totalPresure)
    |> ignore

    let valves =
        match state.openValves |> Map.toList with
        | [] -> "No valves are open."
        | valves ->
            let currentPresure = currentPressure problem state

            let valves =
                valves
                |> List.map (fun (valve, valveRemaining) ->
                    sprintf "%s (%d)" valve (valveRemaining - state.remainingTime))
                |> String.concat ", "

            sprintf "Open valves: %s; releasing %d pressure." valves currentPresure

    sb.AppendLine(valves) |> ignore
    sb.ToString()

let printPathNode problem (node: SearchPathNode<NodeLabel, string, NodeState>) =
    let remaining = node.current.state.remainingTime
    let total = totalPressure problem node.current.state

    let current =
        currentPressure problem node.current.state

    let valve = node.current.node

    let status =
        if isOpen valve node.current.state then
            "+"
        else
            "-"

    let edgeLabel =
        match node.previous with
        | None -> "starting"
        | Some x -> x.edgeLabel

    let openValves =
        node.current.state.openValves
        |> Map.toList
        |> List.map fst
        |> String.concat ","

    let path =
        node
        |> buildAStarPath
        |> List.map (fun x -> sprintf "%s[%d]" x.node (totalPressure problem x.state))
        |> String.concat "->"

    $"%2d{remaining} rem. %s{status}%s{valve}[%5d{total}]: %12s{edgeLabel}. Releasing [%4d{current}] with %-17s{openValves}. Path {path}"
    // $"%2d{remaining} %A{valve}[%5d{total}]: {path}"

let printEdge problem (edge: SearchEdge<NodeLabel, string, NodeState>) =
    let current = currentPressure problem edge.state
    let total = totalPressure problem edge.state

    let valve = edge.target

    let status =
        if isOpen valve edge.state then
            "open"
        else
            "closed"

    $"      - %A{valve}[%5d{total}]: %12s{edge.edgeLabel} [%4d{current}]"

let maximizePressureRelease (problem: Problem) =
    let startState =
        { remainingTime = RemainingTime
          openValves = Map.empty }

    let startNodes =
        [ { node = StartingValve
            state = startState
            isSolution = false } ]

    let stateComparer =
        { new IComparer<NodeState> with
            member _.Compare(x, y) =
                let xPressure = totalPressure problem x
                let yPressure = totalPressure problem y
                - sign(xPressure - yPressure) }

    let stateCost = totalPressure problem >> string

    let nodeComparer = EqualityComparer<NodeLabel>.Default

    let next (current: SearchNode<NodeLabel, NodeState>) =
        seq {
            let valve = current.node
            let state = current.state
            let valveInfo = problem.[valve]
            let valveCount = problem |> Map.count

            if not current.isSolution then
                // If current valve is closed, an option is to take one minute to open it.
                if not (isOpen valve state) then
                    // But only if it is not damaged.
                    if valveInfo.flowRate > 0<flowRate> then
                        // Remaining time releasing pressure after opening it
                        let remainingTime = state.remainingTime - TimeToOpenValve
                        // Mark valve as open
                        let openValves =
                            state.openValves |> Map.add valve remainingTime

                        let isSolution =
                            remainingTime = 1<minute>
                            || openValves |> Map.count = valveCount

                        let state' =
                            { openValves = openValves
                              remainingTime = remainingTime }

                        let edge =
                            { target = valve
                              isSolution = isSolution
                              state = state'
                              edgeLabel = $"open %A{valve}" }

                        printfn "%s" (printEdge problem edge)

                        yield edge

                for nextValve in valveInfo.nextValves do
                    // Remaining time after traversing tunnel
                    let remainingTime =
                        state.remainingTime - TimeToTraverseTunnel

                    let isSolution =
                        remainingTime = 1<minute>
                        || state.openValves |> Map.count = valveCount

                    let state' =
                        { openValves = state.openValves
                          remainingTime = remainingTime }

                    let edge =
                        { target = nextValve
                          isSolution = isSolution
                          state = state'
                          edgeLabel = $"move to %A{nextValve}" }

                    printfn "%s" (printEdge problem edge)

                    yield edge

                // Also, we could do nothing
                let remainingTime =
                    state.remainingTime - TimeToTraverseTunnel

                let edge =
                    { target = valve
                      isSolution = false
                      state = { state with remainingTime = remainingTime }
                      edgeLabel = "do nothing" }

                printfn "%s" (printEdge problem edge)

                yield edge

        }

    aStarSeq
        { startNodes = startNodes
          stateComparer = stateComparer
          nodeComparer = nodeComparer
          stateCost = stateCost
          next = next }
    |> Seq.map (fun node ->
        printPathNode problem node |> printfn "%s"
        node)
    |> Seq.filter (fun node -> node.current.isSolution)
    |> Seq.tryHead
    |> Option.map buildAStarPath

// for pathNode in starSeq do
//     let txt =
//         printState problem pathNode.current.node pathNode.current.state

//     printfn "%s" txt

let execute lines =
    let input = parseInput lines
    let problem = toProblem input

    match maximizePressureRelease problem with
    | None -> printfn "PART1: No solution found."
    | Some solution ->
        printfn "PART1:"

        for item in solution do
            let txt = printState problem item.node item.state

            let doing =
                match item.edge with
                | None -> "starting"
                | Some edge -> fst edge

            printfn "%s while %s" txt doing

Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

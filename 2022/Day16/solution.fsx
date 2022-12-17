#r "nuget: FParsec, 1.1.1"
#load "../../modules/Preamble.fsx"
#load "../../modules/Graphs.fsx"

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open FParsec
open Preamble
open Graphs

#nowarn "0025"
open System.Diagnostics

[<Measure>]
type minute

[<Measure>]
type pressure

[<Measure>]
type flowRate = pressure / minute

let TotalTime = 30<minute>
let TickTime = 1<minute>
let StartingValve = "AA"

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

type Problem =
    { valves: Map<Valve, ValveInfo>
      openableCount: int }

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
    |> fun valves ->
        { valves = valves
          openableCount =
            valves.Values
            |> Seq.filter (fun v -> v.flowRate > 0<flowRate>)
            |> Seq.length }

type State =
    { valve: Valve
      time: int<minute>
      // How much time each valve will remain open
      openValves: Map<Valve, int<minute>>
      previous: State option }

type Decision =
    | OpenValve
    | MoveToValve of Valve
    override this.ToString() =
        match this with
        | OpenValve -> "o>"
        | MoveToValve _ -> "m>"

let isOpen valve state =
    state.openValves |> Map.containsKey valve

let totalPressure (problem: Problem) state =
    state.openValves
    |> Map.toList
    |> List.map (fun (valve, remaining) ->
        problem.valves.[valve].flowRate
        * (TotalTime - remaining))
    |> List.sum

let currentPressure (problem: Problem) state =
    state.openValves
    |> Map.toList
    |> List.map (fun (valve, _) -> problem.valves.[valve].flowRate)
    |> List.sum

let stateCost problem state = -totalPressure problem state

let isSolution problem state =
    state.time = TotalTime
    || state.openValves |> Map.count = problem.openableCount

let openStatus state =
    if isOpen state.valve state then
        "+"
    else
        "-"

let printState problem state =
    // Format: <Time> "<ValveN>" [Pressure]: +-<Valve1>->+-<Valve2>->...+-<ValveN>

    let time = state.time
    let indent = String(' ', int time)
    let pressure = totalPressure problem state
    let valve = state.valve
    let status = openStatus state

    let path =
        let rec loop state =
            match state.previous with
            | None -> [ state ]
            | Some previous -> state :: loop previous

        loop state
        |> Seq.rev
        |> Seq.map (fun state -> $"%s{openStatus state}%s{state.valve}")
        |> String.concat "->"

    sprintf "%s%2d %s%A [%5d]: %s" indent time status valve pressure path

let printSolution part problem solution =
    match solution with
    | None -> printfn "PART %d: No solution found" part
    | Some path ->
        printfn "PART %d: " part

        for item in path do
            let sol = if item.isSolution then "* " else "  "
            let pressure = totalPressure problem item.state
            let status = openStatus item.state
            let valve = item.state.valve

            let decision =
                match item.decision with
                | None -> "Starting"
                | Some OpenValve -> "Open"
                | Some (MoveToValve valve) -> $"Move to {valve}"

            printfn "%s %s%A[%d] %s" sol status valve pressure decision

let searchOptions problem =
    let startState =
        { valve = StartingValve
          time = 0<minute>
          openValves = Map.empty
          previous = None }

    { startNodes =
        [ { state = startState
            isSolution = isSolution problem startState } ]
      stateCostComparer = fun x y -> sign (stateCost problem x - stateCost problem y)
      stateEquals = fun x y -> x.valve = y.valve && x.openValves = y.openValves && x.time = y.time
      stateHash = fun x -> HashCode.Combine(x.valve, x.openValves, x.time)
      next =
        fun node ->
            seq {
                if not node.isSolution then
                    if
                        not (isOpen node.state.valve node.state)
                        && problem.valves.[node.state.valve].flowRate > 0<flowRate>
                    then
                        let state' =
                            { node.state with
                                previous = Some node.state
                                time = node.state.time + TickTime
                                openValves =
                                    node.state.openValves
                                    |> Map.add node.state.valve (node.state.time + TickTime) }

                        yield
                            { decision = OpenValve
                              state = state'
                              isSolution = isSolution problem state' }

                    for valve' in problem.valves.[node.state.valve].nextValves do
                        let state' =
                            { node.state with
                                previous = Some node.state
                                time = node.state.time + TickTime
                                valve = valve' }

                        yield
                            { decision = MoveToValve valve'
                              state = state'
                              isSolution = isSolution problem state' }
            } }

let part1 (problem: Problem) =
    let options = searchOptions problem

    let mutable count = 0
    let mutable bestSolutionPressure = 0<pressure>
    let mutable startTick = Stopwatch.GetTimestamp()

    depthFirstSearchMemoSeq options
    |> Seq.map (fun pathNode ->
        count <- count + 1

        if pathNode.current.isSolution then
            let pressure =
                totalPressure problem pathNode.current.state

            if pressure > bestSolutionPressure then
                bestSolutionPressure <- pressure
                let now = Stopwatch.GetTimestamp()

                let elapsed =
                    int (
                        Stopwatch
                            .GetElapsedTime(
                                startTick,
                                now
                            )
                            .TotalMilliseconds
                    )

                printfn "Found solution with pressure %5d after %6d ms (%8d nodes)" pressure elapsed count

        pathNode)
    |> findBestSolution options.stateCostComparer
    |> Option.map buildPathItems
    |> printSolution 1 problem

let execute lines =
    let input = parseInput lines
    let problem = toProblem input

    part1 problem

Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

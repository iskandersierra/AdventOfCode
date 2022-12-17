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
open System.Collections

[<Measure>]
type minute

[<Measure>]
type pressure

[<Measure>]
type flowRate = pressure / minute

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
    override this.ToString() =
        sprintf "Valve %s has flow rate %2d and connects to %s" this.name this.flowRate (String.concat ", " this.nextValves)

type Problem =
    { valves: Map<Valve, ValveInfo>
      openableCount: int
      totalTime: int<minute> }
    override this.ToString() =
        let valves =
            this.valves
            |> Map.toSeq
            |> Seq.map (fun (name, info) ->
                sprintf "  - %O" info)
            |> String.concat "\n"
        sprintf "Problem with %d meaningful valves out of %d total valves to run for %d minutes\n%s" this.openableCount this.valves.Count this.totalTime valves

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

let toProblem totalTime (ProblemInput lines) : Problem =
    lines
    |> List.map (fun (ValveInput (name, nextValves, flowRate)) ->
        name,
        { name = name
          flowRate = flowRate
          nextValves = nextValves })
    |> Map.ofList
    |> fun valves ->
        { valves = valves
          totalTime = totalTime
          openableCount =
            valves.Values
            |> Seq.filter (fun v -> v.flowRate > 0<flowRate>)
            |> Seq.length }

module Part1 =
    type State =
        { valve: Valve
          time: int<minute>
          totalPressure: int<pressure>
          // How much time each valve will remain open
          openValves: Set<Valve>
          previous: State option }

    type Decision =
        | OpenValve
        | MoveToValve of Valve

    let isOpen openValves valve = openValves |> Set.contains valve

    let stateCost state = -state.totalPressure

    let isSolution problem openValves time =
        time = problem.totalTime
        || Set.count openValves = problem.openableCount

    let openStatus openValves valve =
        if isOpen openValves valve then
            "+"
        else
            "-"

    let decisionStatus decision =
        match decision with
        | None -> "Do nothing"
        | Some OpenValve -> "Open valve"
        | Some (MoveToValve valve) -> $"Move to {valve}"

    let printSolution solution =
        match solution with
        | None -> printfn "PART 1: No solution found"
        | Some path ->
            printfn "PART 1: "

            for item in path do
                let sol = if item.isSolution then "* " else "  "
                let pressure = item.state.totalPressure

                let status =
                    openStatus item.state.openValves item.state.valve

                let valve = item.state.valve
                let decision = decisionStatus item.decision
                printfn "%s %s%A[%d] %s" sol status valve pressure decision

    let getPossibleDecisions problem openValves valve =
        seq {
            if
                not (isOpen openValves valve)
                && problem.valves.[valve].flowRate > 0<flowRate>
            then
                yield OpenValve

            for valve' in problem.valves.[valve].nextValves do
                yield MoveToValve valve'
        }

    let applyDecision problem state decision =
        match decision with
        | OpenValve ->
            let time = state.time + TickTime

            { state with
                previous = Some state
                time = time
                totalPressure =
                    state.totalPressure
                    + problem.valves.[state.valve].flowRate
                      * (problem.totalTime - time)
                openValves = state.openValves |> Set.add state.valve }

        | MoveToValve valve ->
            { state with
                previous = Some state
                time = state.time + TickTime
                valve = valve }

    let searchOptions problem =
        let startState =
            { valve = StartingValve
              time = 0<minute>
              totalPressure = 0<pressure>
              openValves = Set.empty
              previous = None }

        { startNodes =
            [ { state = startState
                isSolution = isSolution problem startState.openValves startState.time } ]
          stateCostComparer = fun x y -> sign (stateCost x - stateCost y)
          stateEquals =
            fun x y ->
                x.valve = y.valve
                && x.openValves = y.openValves
                && x.totalPressure = y.totalPressure
          stateHash = fun x -> HashCode.Combine(x.valve, x.openValves, x.totalPressure)
          next =
            fun node ->
                seq {
                    if not node.isSolution then
                        for decision in getPossibleDecisions problem node.state.openValves node.state.valve do
                            let state =
                                applyDecision problem node.state decision

                            yield
                                { decision = decision
                                  state = state
                                  isSolution = isSolution problem state.openValves state.time }

                }

        }

    let execute (problem: Problem) =
        let options = searchOptions problem

        let mutable startTick = Stopwatch.GetTimestamp()

        breadthFirstSearchMemoSeq options
        |> findBestSolution options.stateCostComparer
        |> Option.map buildPathItems
        |> printSolution

        let elapsed =
            Stopwatch.GetElapsedTime(startTick, Stopwatch.GetTimestamp())

        printfn "Elapsed: %A" elapsed

module Part2 =

    type State =
        { myValve: Valve
          elephantValve: Valve
          time: int<minute>
          totalPressure: int<pressure>
          // How much time each valve will remain open
          openValves: Set<Valve>
          previous: State option }

    type Decision =
        { myDecision: Part1.Decision
          elephantDecision: Part1.Decision }

    let stateCost state = -state.totalPressure

    let decisionStatus decision =
        match decision with
        | None -> "Do nothing"
        | Some decision ->
            let myDecision =
                Part1.decisionStatus (Some decision.myDecision)

            let elephantDecision =
                Part1.decisionStatus (Some decision.elephantDecision)

            $"I {myDecision} / Elephant {elephantDecision}"

    let printSolution solution =
        match solution with
        | None -> printfn "PART 2: No solution found"
        | Some path ->
            printfn "PART 2: "

            for item in path do
                let sol = if item.isSolution then "* " else "  "
                let pressure = item.state.totalPressure
                let myValve = item.state.myValve
                let elephantValve = item.state.elephantValve

                let myStatus =
                    Part1.openStatus item.state.openValves item.state.myValve

                let elephantStatus =
                    Part1.openStatus item.state.openValves item.state.elephantValve

                let decision = decisionStatus item.decision

                printfn "%s %s%A|%s%A[%d] %s" sol myStatus myValve elephantStatus elephantValve pressure decision

    let applyDecision problem state decision =
        match decision.myDecision, decision.elephantDecision with
        | Part1.OpenValve, Part1.OpenValve ->
            if state.myValve = state.elephantValve then
                None
            else
                let time = state.time + TickTime

                Some
                    { state with
                        previous = Some state
                        time = time
                        totalPressure =
                            state.totalPressure
                            + problem.valves.[state.myValve].flowRate
                              * (problem.totalTime - time)
                            + problem.valves.[state.elephantValve].flowRate
                              * (problem.totalTime - time)
                        openValves =
                            state.openValves
                            |> Set.add state.myValve
                            |> Set.add state.elephantValve }

        | Part1.MoveToValve myValve, Part1.OpenValve ->
            let time = state.time + TickTime

            Some
                { state with
                    previous = Some state
                    time = time
                    myValve = myValve
                    totalPressure =
                        state.totalPressure
                        + problem.valves.[state.elephantValve].flowRate
                          * (problem.totalTime - time)
                    openValves = state.openValves |> Set.add state.elephantValve }

        | Part1.OpenValve, Part1.MoveToValve elephantValve ->
            let time = state.time + TickTime

            Some
                { state with
                    previous = Some state
                    time = time
                    elephantValve = elephantValve
                    totalPressure =
                        state.totalPressure
                        + problem.valves.[state.myValve].flowRate
                          * (problem.totalTime - time)
                    openValves = state.openValves |> Set.add state.myValve }

        | Part1.MoveToValve myValve, Part1.MoveToValve elephantValve ->
            Some
                { state with
                    previous = Some state
                    time = state.time + TickTime
                    myValve = myValve
                    elephantValve = elephantValve }

    let searchOptions problem =
        let startState =
            { myValve = StartingValve
              elephantValve = StartingValve
              time = 0<minute>
              totalPressure = 0<pressure>
              openValves = Set.empty
              previous = None }

        { startNodes =
            [ { state = startState
                isSolution = Part1.isSolution problem startState.openValves startState.time } ]
          stateCostComparer = fun x y -> sign (stateCost x - stateCost y)
          stateEquals =
            fun x y ->
                x.myValve = y.myValve
                && x.elephantValve = y.elephantValve
                && x.openValves = y.openValves
                && x.totalPressure = y.totalPressure
          stateHash = fun x -> HashCode.Combine(x.myValve, x.elephantValve, x.openValves, x.totalPressure)
          next =
            fun node ->
                seq {
                    if not node.isSolution then
                        for myDecision in Part1.getPossibleDecisions problem node.state.openValves node.state.myValve do
                            for elephantDecision in Part1.getPossibleDecisions problem node.state.openValves node.state.elephantValve do
                                let decision = { myDecision = myDecision; elephantDecision = elephantDecision }
                                match applyDecision problem node.state decision with
                                | None -> ()
                                | Some state ->
                                    yield
                                        { decision = decision
                                          state = state
                                          isSolution = Part1.isSolution problem state.openValves state.time }
                }
        }

    let execute (problem: Problem) =
        let options = searchOptions problem

        let mutable startTick = Stopwatch.GetTimestamp()

        breadthFirstSearchMemoSeq options
        |> findBestSolution options.stateCostComparer
        |> Option.map buildPathItems
        |> printSolution

        let elapsed =
            Stopwatch.GetElapsedTime(startTick, Stopwatch.GetTimestamp())

        printfn "Elapsed: %A" elapsed

let execute lines =
    let input = parseInput lines

    let problem1 = toProblem 30<minute> input
    printfn "Problem: %O" problem1
    Part1.execute problem1

    let problem2 = toProblem 26<minute> input
    Part2.execute problem2

Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

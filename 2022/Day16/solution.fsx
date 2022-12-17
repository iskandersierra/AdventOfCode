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
      index: int
      flowRate: int<flowRate>
      nextValves: {| index: int
                     distance: int<minute>
                     name: string |} [] }
    override this.ToString() =
        let nextValves =
            this.nextValves
            |> Seq.map (fun next -> sprintf "%s [%d] at %d min" next.name next.index next.distance)
            |> String.concat ", "

        sprintf "Valve %s [%d] has flow rate %2d and connects to %s" this.name this.index this.flowRate nextValves

type Problem =
    { valves: ValveInfo []
      startIndex: int
      openableCount: int
      totalTime: int<minute> }
    override this.ToString() =
        let valves =
            this.valves
            |> Seq.map (fun info -> sprintf "  - %O" info)
            |> String.concat "\n"

        sprintf
            "Problem with %d meaningful valves out of %d total valves to run for %d minutes\n%s"
            this.openableCount
            this.valves.Length
            this.totalTime
            valves

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
    // Extract all the meaningful valves, meaning those with a flow rate > 0 or that are the starting valve
    // Compute the distance from each meaningful valve to the rest of meaningful valves
    // Use the compressed graph to enhance performance
    // EX:
    // AA[0] <-> BB[13] <-> CC[2] <-> DD[20] <-> EE[3] <-> FF[0] <-> GG[0] <-> HH[22]
    // AA[0] <-> II[0] <-> JJ[21]
    // AA[0] <-> DD[20]
    // Shuld be compressed to:
    // AA[0] <-1-> BB[13] <-1-> CC[2] <-1-> DD[20] <-1-> EE[3] <-3-> HH[22]
    // AA[0] <-2-> JJ[21]
    // AA[0] <-1-> DD[20]

    let valveMap =
        lines
        |> Seq.map (fun (ValveInput (name, nextValves, flowRate)) ->
            name,
            {| nextValves = nextValves
               flowRate = flowRate |})
        |> Map.ofSeq

    // Create an array of good valves, with an index from 0 to 31 at most
    let goodValves =
        lines
        |> Seq.filter (fun (ValveInput (name, _, flowRate)) -> flowRate > 0<flowRate> || name = StartingValve)
        |> Seq.mapi (fun index (ValveInput (name, nextValves, flowRate)) ->
            { name = name
              index = index
              flowRate = flowRate
              nextValves = [||] })
        |> Seq.toArray

    let nameToIndexMap =
        valveMap.Keys
        |> Seq.mapi (fun mapIndex name ->
            match goodValves
                  |> Array.tryFindIndex (fun goodValve -> goodValve.name = name)
                with
            | Some index -> name, index
            | None -> name, -mapIndex)
        |> Map.ofSeq

    let indexToNameMap =
        nameToIndexMap
        |> Map.toSeq
        |> Seq.map (fun (index, name) -> name, index)
        |> Map.ofSeq

    for index in 0 .. goodValves.Length - 1 do
        let info = goodValves.[index]

        let distances =
            breadthFirstSearchMemoSeq
                { startNodes =
                    [ { state =
                          {| name = info.name
                             index = index
                             steps = 0<minute> |}
                        isSolution = true } ]
                  stateCostComparer = fun _ _ -> 0
                  stateEquals = fun a b -> a.index = b.index
                  stateHash = fun a -> hash a.index
                  next =
                    fun node ->
                        let state = node.state

                        valveMap.[state.name].nextValves
                        |> Seq.map (fun next ->
                            { state =
                                {| name = next
                                   index =
                                    nameToIndexMap
                                    |> Map.tryFind next
                                    |> function
                                        | Some x -> x
                                        | None -> -1
                                   steps = state.steps + TickTime |}
                              decision = ()
                              isSolution = false }) }
            |> Seq.map (fun node -> node.current.state.index, node.current.state.steps)

        let nextValves =
            distances
            |> Seq.map (fun (index, distance) ->
                let name = indexToNameMap.[index]

                {| name = name
                   index = index
                   distance = distance |})
            |> Seq.filter (fun info -> info.index >= 0 && info.distance > 0<minute>)
            |> Seq.toArray

        goodValves.[index] <- { info with nextValves = nextValves }

    let openableCount =
        goodValves
        |> Seq.filter (fun info -> info.flowRate > 0<flowRate>)
        |> Seq.length

    { valves = goodValves
      startIndex = nameToIndexMap.[StartingValve]
      openableCount = openableCount
      totalTime = totalTime }

type IndexSet = uint32

let isOpen (valveIndex: int) (openValves: IndexSet) =
    (openValves &&& (1u <<< valveIndex)) <> 0u

let addOpenValve (valveIndex: int) (openValves: IndexSet) = openValves ||| (1u <<< valveIndex)

module Part1 =
    type State =
        { valve: Valve
          index: int
          time: int<minute>
          totalPressure: int<pressure>
          openValvesCount: int
          openValves: IndexSet
          previous: State option }

    type Decision =
        | MoveAndOpen of index: int * steps: int<minute>
        | Solved

    let stateCost state = -state.totalPressure

    let isSolution problem state =
        state.time = problem.totalTime
        || state.openValvesCount = problem.openableCount

    let openStatus state =
        if isOpen state.index state.openValves then
            "+"
        else
            "-"

    let decisionStatus problem decision =
        match decision with
        | None -> "Starting"
        | Some Solved -> "Solved"
        | Some (MoveAndOpen (index, steps)) ->
            let name = problem.valves.[index].name
            $"Move and open {name} in {steps} min"

    let printSolution problem solution =
        match solution with
        | None -> printfn "PART 1: No solution found"
        | Some path ->
            printfn "PART 1: "

            for item in path do
                let pressure = item.state.totalPressure
                let status = openStatus item.state
                let valve = item.state.valve
                let decision = decisionStatus problem item.decision
                printfn "%s%A[%d] %s" status valve pressure decision

    let getPossibleDecisions problem (openValves: IndexSet) valveIndex =
        seq {
            for next in problem.valves.[valveIndex].nextValves do
                if
                    not (isOpen next.index openValves)
                    && problem.valves.[next.index].flowRate > 0<flowRate>
                then
                    yield MoveAndOpen(next.index, next.distance)
        }

    let applyDecision problem state decision : SearchEdge<State, Decision> option =
        match decision with
        | MoveAndOpen (index, steps) ->
            let time = state.time + steps + TickTime

            if time > problem.totalTime then
                None
            else
                let state' =
                    { state with
                        previous = Some state
                        time = time
                        index = index
                        valve = problem.valves.[index].name
                        totalPressure =
                            state.totalPressure
                            + problem.valves.[index].flowRate
                              * (problem.totalTime - time)
                        openValvesCount = state.openValvesCount + 1
                        openValves = state.openValves |> addOpenValve index }

                Some
                    { decision = decision
                      state = state'
                      isSolution = isSolution problem state' }

        | Solved -> None

    let searchOptions problem =
        let startState =
            { valve = StartingValve
              index = problem.startIndex
              time = 0<minute>
              totalPressure = 0<pressure>
              openValvesCount = 0
              openValves = 0u
              previous = None }

        { startNodes =
            [ { state = startState
                isSolution = isSolution problem startState } ]
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
                        for decision in getPossibleDecisions problem node.state.openValves node.state.index do
                            match applyDecision problem node.state decision with
                            | Some edge -> yield edge
                            | None -> ()
                }
        }

    let execute (problem: Problem) =
        let options = searchOptions problem

        let mutable startTick = Stopwatch.GetTimestamp()

        breadthFirstSearchMemoSeq options
        |> findBestSolution options.stateCostComparer
        |> Option.map buildPathItems
        |> printSolution problem

        let elapsed =
            Stopwatch.GetElapsedTime(startTick, Stopwatch.GetTimestamp())

        printfn "Elapsed: %A" elapsed

// module Part2 =

//     type State =
//         { myValve: Valve
//           elephantValve: Valve
//           time: int<minute>
//           totalPressure: int<pressure>
//           // How much time each valve will remain open
//           openValves: Set<Valve>
//           previous: State option }

//     type Decision =
//         { myDecision: Part1.Decision
//           elephantDecision: Part1.Decision }

//     let stateCost state = -state.totalPressure

//     let decisionStatus decision =
//         match decision with
//         | None -> "Do nothing"
//         | Some decision ->
//             let myDecision =
//                 Part1.decisionStatus (Some decision.myDecision)

//             let elephantDecision =
//                 Part1.decisionStatus (Some decision.elephantDecision)

//             $"I {myDecision} / Elephant {elephantDecision}"

//     let printSolution solution =
//         match solution with
//         | None -> printfn "PART 2: No solution found"
//         | Some path ->
//             printfn "PART 2: "

//             for item in path do
//                 let sol = if item.isSolution then "* " else "  "
//                 let pressure = item.state.totalPressure
//                 let myValve = item.state.myValve
//                 let elephantValve = item.state.elephantValve

//                 let myStatus =
//                     Part1.openStatus item.state.openValves item.state.myValve

//                 let elephantStatus =
//                     Part1.openStatus item.state.openValves item.state.elephantValve

//                 let decision = decisionStatus item.decision

//                 printfn "%s %s%A|%s%A[%d] %s" sol myStatus myValve elephantStatus elephantValve pressure decision

//     let applyDecision problem state decision =
//         match decision.myDecision, decision.elephantDecision with
//         | Part1.OpenValve, Part1.OpenValve ->
//             if state.myValve = state.elephantValve then
//                 None
//             else
//                 let time = state.time + TickTime

//                 Some
//                     { state with
//                         previous = Some state
//                         time = time
//                         totalPressure =
//                             state.totalPressure
//                             + problem.valves.[state.myValve].flowRate
//                               * (problem.totalTime - time)
//                             + problem.valves.[state.elephantValve].flowRate
//                               * (problem.totalTime - time)
//                         openValves =
//                             state.openValves
//                             |> Set.add state.myValve
//                             |> Set.add state.elephantValve }

//         | Part1.MoveToValve myValve, Part1.OpenValve ->
//             let time = state.time + TickTime

//             Some
//                 { state with
//                     previous = Some state
//                     time = time
//                     myValve = myValve
//                     totalPressure =
//                         state.totalPressure
//                         + problem.valves.[state.elephantValve].flowRate
//                           * (problem.totalTime - time)
//                     openValves = state.openValves |> Set.add state.elephantValve }

//         | Part1.OpenValve, Part1.MoveToValve elephantValve ->
//             let time = state.time + TickTime

//             Some
//                 { state with
//                     previous = Some state
//                     time = time
//                     elephantValve = elephantValve
//                     totalPressure =
//                         state.totalPressure
//                         + problem.valves.[state.myValve].flowRate
//                           * (problem.totalTime - time)
//                     openValves = state.openValves |> Set.add state.myValve }

//         | Part1.MoveToValve myValve, Part1.MoveToValve elephantValve ->
//             Some
//                 { state with
//                     previous = Some state
//                     time = state.time + TickTime
//                     myValve = myValve
//                     elephantValve = elephantValve }

//     let searchOptions problem =
//         let startState =
//             { myValve = StartingValve
//               elephantValve = StartingValve
//               time = 0<minute>
//               totalPressure = 0<pressure>
//               openValves = Set.empty
//               previous = None }

//         { startNodes =
//             [ { state = startState
//                 isSolution = Part1.isSolution problem startState.openValves startState.time } ]
//           stateCostComparer = fun x y -> sign (stateCost x - stateCost y)
//           stateEquals =
//             fun x y ->
//                 x.myValve = y.myValve
//                 && x.elephantValve = y.elephantValve
//                 && x.openValves = y.openValves
//                 && x.totalPressure = y.totalPressure
//           stateHash = fun x -> HashCode.Combine(x.myValve, x.elephantValve, x.openValves, x.totalPressure)
//           next =
//             fun node ->
//                 seq {
//                     if not node.isSolution then
//                         for myDecision in Part1.getPossibleDecisions problem node.state.openValves node.state.myValve do
//                             for elephantDecision in Part1.getPossibleDecisions problem node.state.openValves node.state.elephantValve do
//                                 let decision = { myDecision = myDecision; elephantDecision = elephantDecision }
//                                 match applyDecision problem node.state decision with
//                                 | None -> ()
//                                 | Some state ->
//                                     yield
//                                         { decision = decision
//                                           state = state
//                                           isSolution = Part1.isSolution problem state.openValves state.time }
//                 }
//         }

//     let execute (problem: Problem) =
//         let options = searchOptions problem

//         let mutable startTick = Stopwatch.GetTimestamp()

//         breadthFirstSearchMemoSeq options
//         |> findBestSolution options.stateCostComparer
//         |> Option.map buildPathItems
//         |> printSolution

//         let elapsed =
//             Stopwatch.GetElapsedTime(startTick, Stopwatch.GetTimestamp())

//         printfn "Elapsed: %A" elapsed

let execute lines =
    let input = parseInput lines

    let problem1 = toProblem 30<minute> input
    printfn "Problem: %O\n" problem1
    Part1.execute problem1

// let problem2 = toProblem 26<minute> input
// Part2.execute problem2

Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

#r "nuget: FParsec, 1.1.1"
#load "../../modules/Preamble.fsx"
#load "../../modules/Graphs.fsx"

open System
open System.Collections
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Text
open System.Text.RegularExpressions
open FParsec
open Preamble
open Graphs

#nowarn "0025"

let TotalTime = 20

type Blueprint =
    { index: int
      oreOre: int
      clayOre: int
      obsidianOre: int
      obsidianClay: int
      geodeOre: int
      geodeObsidian: int }

type ProblemInput = Blueprint []

let parseInput lines : ProblemInput =
    let parser =
        let parseBlueprintHeader = pstring "Blueprint " >>. (pint32 .>> pstring ":")
        let parseOreRobot = pstring "Each ore robot costs " >>. (pint32 .>> pstring " ore.")
        let parseClayRobot = pstring "Each clay robot costs " >>. (pint32 .>> pstring " ore.")
        let parseObsidianRobot =
            pipe2
                (pstring "Each obsidian robot costs " >>. (pint32 .>> pstring " ore and "))
                (pint32 .>> pstring " clay.")
                (fun ore clay -> (ore, clay))
        let parseGeodeRobot =
            pipe2
                (pstring "Each geode robot costs " >>. (pint32 .>> pstring " ore and "))
                (pint32 .>> pstring " obsidian.")
                (fun ore obsidian -> (ore, obsidian))
        let parseBlueprint =
            pipe5
                (parseBlueprintHeader .>> spaces)
                (parseOreRobot .>> spaces)
                (parseClayRobot .>> spaces)
                (parseObsidianRobot .>> spaces)
                (parseGeodeRobot .>> spaces)
                (fun index oreOre clayOre (obsidianOre, obsidianClay) (geodeOre, geodeObsidian) ->
                    { index = index
                      oreOre = oreOre
                      clayOre = clayOre
                      obsidianOre = obsidianOre
                      obsidianClay = obsidianClay
                      geodeOre = geodeOre
                      geodeObsidian = geodeObsidian })

        many1 parseBlueprint |>> Array.ofList

    match run parser lines with
    | Success (result, _, _) -> result
    | Failure (message, _, _) -> failwith message

type State1 =
    { time: int
      ore: int
      oreRobots: int
      clay: int
      clayRobots: int
      obsidian: int
      obsidianRobots: int
      geode: int
      geodeRobots: int }

let runBlueprintPart1 blueprint =
    printfn "Running blueprint %d:" blueprint.index

    let getChildren state =
        seq {
            // When no new robots are built
            yield { state with
                        time = state.time + 1
                        ore = state.ore + state.oreRobots
                        clay = state.clay + state.clayRobots
                        obsidian = state.obsidian + state.obsidianRobots
                        geode = state.geode + state.geodeRobots }

            // When a new ore robot is built
            if state.ore >= blueprint.oreOre then
                yield { state with
                            time = state.time + 1
                            oreRobots = state.oreRobots + 1
                            ore = state.ore + state.oreRobots - blueprint.oreOre
                            clay = state.clay + state.clayRobots
                            obsidian = state.obsidian + state.obsidianRobots
                            geode = state.geode + state.geodeRobots  }

            // When a new clay robot is built
            if state.ore >= blueprint.clayOre then
                yield { state with
                            time = state.time + 1
                            clayRobots = state.clayRobots + 1
                            ore = state.ore + state.oreRobots - blueprint.clayOre
                            clay = state.clay + state.clayRobots
                            obsidian = state.obsidian + state.obsidianRobots
                            geode = state.geode + state.geodeRobots  }

            // When a new obsidian robot is built
            if state.ore >= blueprint.obsidianOre && state.clay >= blueprint.obsidianClay then
                yield { state with
                            time = state.time + 1
                            obsidianRobots = state.obsidianRobots + 1
                            ore = state.ore + state.oreRobots - blueprint.obsidianOre
                            clay = state.clay + state.clayRobots - blueprint.obsidianClay
                            obsidian = state.obsidian + state.obsidianRobots
                            geode = state.geode + state.geodeRobots  }

            // When a new geode robot is built
            if state.ore >= blueprint.geodeOre && state.obsidian >= blueprint.geodeObsidian then
                yield { state with
                            time = state.time + 1
                            geodeRobots = state.geodeRobots + 1
                            ore = state.ore + state.oreRobots - blueprint.geodeOre
                            clay = state.clay + state.clayRobots
                            obsidian = state.obsidian + state.obsidianRobots - blueprint.geodeObsidian
                            geode = state.geode + state.geodeRobots  }
        }

    let rec loop state =
        if state.time >= TotalTime then
            state.geode
        else
            getChildren state
            |> Array.ofSeq
            |> Array.Parallel.map (fun s -> loop s)
            |> Array.max

    let initState =
        { time = 0
          ore = 0
          oreRobots = 1
          clay = 0
          clayRobots = 0
          obsidian = 0
          obsidianRobots = 0
          geode = 0
          geodeRobots = 0 }

    loop initState

let part1 input =
    input
    |> Array.map (fun b ->
        let result = runBlueprintPart1 b
        printfn "  Geode = %d * %d = %d" result b.index (result * b.index)
        result * b.index)
    |> Array.sum

let execute lines =
    let input = parseInput lines

    printfn "Problem: %A\n" input

    let stopwatch = Stopwatch.StartNew()

    let result = part1 input

    printfn "Elapsed: %A" stopwatch.Elapsed
    printfn "RESULT PART1: %d" result

    stopwatch.Restart()

    printfn "Elapsed: %A" stopwatch.Elapsed
    printfn "RESULT PART2: %d" 2


Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

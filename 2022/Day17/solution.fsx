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


[<Measure>]
type minute

[<Measure>]
type pressure

[<Measure>]
type flowRate = pressure / minute

[<Literal>]
let PushLeft = '<'

[<Literal>]
let PushRight = '>'

type Rock = int * byte[]

type ProblemInput = string

type ProblemSpec =
    { rocks: Rock[]
      chamberWidth: int
      leftOffset: int
      bottomOffset: int
      fallingRocksCount: int }

type Problem =
    { streams: string
      spec: ProblemSpec }

let parseInput lines = lines

let toProblem spec streams : Problem =
    { streams = streams
      spec = spec }

module Rocks =
    // ####
    let rock1: Rock = 4, [|15uy|]

    // .#.
    // ###
    // .#.
    let rock2: Rock = 3, [|2uy; 7uy; 2uy|]

    // ..#
    // ..#
    // ###
    let rock3: Rock = 3, [|1uy; 1uy; 7uy|]

    // #
    // #
    // #
    // #
    let rock4: Rock = 1, [|1uy; 1uy; 1uy; 1uy|]

    // ##
    // ##
    let rock5: Rock = 2, [|3uy; 3uy|]

module Part1 =
    let createSpec() =
        { rocks = [| Rocks.rock1; Rocks.rock2; Rocks.rock3; Rocks.rock4; Rocks.rock5 |]
          chamberWidth = 7
          leftOffset = 2
          bottomOffset = 3
          fallingRocksCount = 8 }

    let execute problem =
        let stopwatch = Stopwatch.StartNew()

        let spec = problem.spec
        let rockHeightSum = spec.rocks |> Array.sumBy (fun (h, _) -> h)
        let tallestRockHeight = spec.rocks |> Array.maxBy (fun (h, _) -> h) |> fst
        let totalRockSetFalls = int (ceil (float spec.fallingRocksCount / float spec.rocks.Length)) * rockHeightSum
        let chamberHeight = spec.bottomOffset + totalRockSetFalls

        // printfn "Rock height sum: %d" rockHeightSum
        // printfn "Tallest rock height: %d" tallestRockHeight
        // printfn "Total rock set falls: %d" totalRockSetFalls
        // printfn "Chamber height: %d" chamberHeight

        let chamber: byte[] = Array.zeroCreate chamberHeight
        let mutable highestRock = 0

        for index in 0 .. spec.fallingRocksCount - 1 do
            let rockIndex = index % spec.rocks.Length
            let (rockWidth, rockData) = spec.rocks.[rockIndex]

            let streamIndex = index % problem.streams.Length
            let stream = problem.streams.[streamIndex]
            // printfn "Falling rock %d (width %d) %A" rockIndex rockWidth rockData
            // printfn "Stream %d %A" streamIndex stream

            let mutable rockLeft = spec.leftOffset
            let mutable rockTop = highestRock + spec.bottomOffset + rockData.Length - 1
            printfn "Rock %d at %d, %d" rockIndex rockLeft rockTop
            let mutable canFall = true

            // while canFall do
            //     match stream with
            //     | PushLeft when canMoveLeft ->


        printfn "PART 1 RESULT: %d" -1
        printfn "Elapsed: %O" stopwatch.Elapsed

let execute lines =
    let input = parseInput lines

    let problem1 = toProblem (Part1.createSpec()) input
    // printfn "Problem: %O\n" problem1
    Part1.execute problem1

// let problem2 = toProblem 26<minute> input
// Part2.execute problem2

Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

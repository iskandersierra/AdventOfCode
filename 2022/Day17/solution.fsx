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

[<Literal>]
let PushLeft = '<'

[<Literal>]
let PushRight = '>'

type Rock =
    { width: int
      height: int
      pattern: uint32 }

type ProblemInput = string

type ProblemSpec =
    { rocks: Rock []
      chamberWidth: int
      leftOffset: int
      bottomOffset: int
      fallingRocksCount: int }

type Problem = { streams: string; spec: ProblemSpec }

let parseInput lines = lines

let toProblem spec streams : Problem = { streams = streams; spec = spec }

module Rocks =
    let Mask = 0b1111_1111u
    let MaskWidth = 8
    let MaxHeight = 4

    // ....
    // ....
    // ....
    // ####
    // ........ ........ ........ ####....
    let rock1: Rock =
        { width = 4
          height = 1
          pattern = 0b00000000_00000000_00000000_11110000u }

    // ....
    // .#..
    // ###.
    // .#..
    // ........ .#...... ###.... .#......
    let rock2: Rock =
        { width = 3
          height = 3
          pattern = 0b00000000_01000000_11100000_01000000u }

    // ....
    // ..#.
    // ..#.
    // ###.
    // ........ ..#..... ..#..... ###.....
    let rock3: Rock =
        { width = 3
          height = 3
          pattern = 0b00000000_00100000_00100000_11100000u }

    // #...
    // #...
    // #...
    // #...
    // #....... #....... #....... #.......
    let rock4: Rock =
        { width = 1
          height = 4
          pattern = 0b10000000_10000000_10000000_10000000u }

    // ....
    // ....
    // ##..
    // ##..
    // ........ ........ ##...... ##......
    let rock5: Rock =
        { width = 2
          height = 2
          pattern = 0b00000000_00000000_11000000_11000000u }

    let inline pickRow index (pattern: uint32) =
        byte ((pattern >>> (index * MaskWidth)) &&& Mask)

    let inline move amount pattern =
        if amount = 0 then pattern
        elif amount > 0 then pattern >>> amount
        else pattern <<< -amount

    let toString pattern =
        let sb = StringBuilder()

        for index = MaxHeight - 1 downto 0 do
            let row = pickRow index pattern

            let txt =
                Convert
                    .ToString(row, 2)
                    .PadLeft(MaskWidth, '0')
                    .Replace('0', '.')
                    .Replace('1', '#')

            sb.AppendLine(txt) |> ignore

        sb.ToString()

// printfn "Rock 1:\n%s" (toString rock1.pattern)
// printfn "Rock 2:\n%s" (toString rock2.pattern)
// printfn "Rock 3:\n%s" (toString rock3.pattern)
// printfn "Rock 4:\n%s" (toString rock4.pattern)
// printfn "Rock 5:\n%s" (toString rock5.pattern)

module Part1 =
    let createSpec () =
        { rocks =
            [| Rocks.rock1
               Rocks.rock2
               Rocks.rock3
               Rocks.rock4
               Rocks.rock5 |]
          chamberWidth = 7
          leftOffset = 2
          bottomOffset = 3
          fallingRocksCount = 2022 }

    let printChamber chamberWidth emptyRow (chamber: byte []) =
        let emptyRow = max emptyRow 6
        let viewEnd = max (emptyRow - 10) 0

        for index in emptyRow .. -1 .. viewEnd do
            let row = chamber.[index]
            let border = if index = emptyRow then " " else "|"
            let txt =
                Convert
                    .ToString(row, 2)
                    .PadLeft(chamberWidth, '0')
                    .Replace('0', '.')
                    .Replace('1', '#')

            printfn "%4d: %s%s%s" index border txt border

        if viewEnd > 0 then
            printfn "      |~~~~~~~|"
        else
            printfn "      +-------+"

    let execute problem =
        let stopwatch = Stopwatch.StartNew()

        let spec = problem.spec

        let rockHeightSum =
            spec.rocks
            |> Seq.map (fun rock -> rock.height)
            |> Seq.sum

        let tallestRockHeight =
            spec.rocks
            |> Seq.map (fun rock -> rock.height)
            |> Seq.max

        let totalRockSetFalls =
            int (
                ceil (
                    float spec.fallingRocksCount
                    / float spec.rocks.Length
                )
            )
            * rockHeightSum

        let chamberHeight = spec.bottomOffset + totalRockSetFalls

        printfn "Stream length: %d" problem.streams.Length
        printfn "Rock height sum: %d" rockHeightSum
        printfn "Tallest rock height: %d" tallestRockHeight
        printfn "Total rock set falls: %d" totalRockSetFalls
        printfn "Chamber height: %d" chamberHeight

        let chamber: byte [] = Array.zeroCreate chamberHeight
        let mutable emptyRow = 0
        let mutable rockIndex = 0
        let mutable streamIndex = 0

        for index = 1 to spec.fallingRocksCount do
            let rock = spec.rocks.[rockIndex]

            // Put rock into position
            let mutable rockLeft = spec.leftOffset
            let mutable rockBottom = emptyRow + spec.bottomOffset

            let mutable rockPattern =
                rock.pattern
                |> Rocks.move (Rocks.MaskWidth - spec.chamberWidth + rockLeft)

            let mutable chamberPattern =
                let mutable pattern = 0u
                for i = rockBottom + 3 downto rockBottom do
                    pattern <- pattern <<< Rocks.MaskWidth ||| uint32 chamber.[i]
                pattern

            let mutable canFall = true

            while canFall do
                // First, try to push the rock left or right using the stream
                // For it, it must be able to move in tha direction without hitting
                // the chamber wall or another rock
                let stream = problem.streams.[streamIndex]
                match stream with
                | PushLeft ->
                    if rockLeft > 0 then
                        let newPattern = rockPattern |> Rocks.move -1
                        if newPattern &&& chamberPattern = 0u then
                            rockPattern <- newPattern
                            rockLeft <- rockLeft - 1
                | PushRight ->
                    if rockLeft + rock.width < spec.chamberWidth then
                        let newPattern = rockPattern |> Rocks.move 1
                        if newPattern &&& chamberPattern = 0u then
                            rockPattern <- newPattern
                            rockLeft <- rockLeft + 1
                | ch ->
                    failwithf "Unexpected stream character: %c at index %d" ch streamIndex

                streamIndex <- (streamIndex + 1) % problem.streams.Length

                // Then, try to push the rock down
                // For it, it must be able to move down without hitting
                // the chamber floor or another rock
                if rockBottom = 0 then
                    canFall <- false
                elif rockBottom > emptyRow then
                    rockBottom <- rockBottom - 1
                    chamberPattern <- chamberPattern <<< Rocks.MaskWidth ||| 0u
                else
                    chamberPattern <- chamberPattern <<< Rocks.MaskWidth ||| uint32 chamber.[rockBottom - 1]
                    if rockPattern &&& chamberPattern = 0u then
                        rockBottom <- rockBottom - 1
                    else
                        canFall <- false

            // Put the rock in the chamber
            for i = 0 to Rocks.MaxHeight - 1 do
                let row = chamber.[rockBottom + i]
                let rockRow = rockPattern |> Rocks.pickRow i
                let newRow = row ||| rockRow
                chamber.[rockBottom + i] <- newRow

            emptyRow <- max emptyRow (rockBottom + rock.height)

            // printfn "\nFallen rocks: %d. Empty from: %d" (index + 1) emptyRow
            // chamber |> printChamber spec.chamberWidth emptyRow

            rockIndex <- (rockIndex + 1) % spec.rocks.Length

        printfn "PART 1 RESULT: %d" emptyRow
        printfn "Elapsed: %O" stopwatch.Elapsed

let execute lines =
    let input = parseInput lines

    let problem1 = toProblem (Part1.createSpec ()) input
    // printfn "Problem: %O\n" problem1
    Part1.execute problem1

Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

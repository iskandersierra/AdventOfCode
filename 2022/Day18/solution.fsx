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

let Air = 0
let Lava = 1
let Water = 2
let Steam = 3
let BoulderSize = 22

type Drop = int * int * int

module Drop =
    let minimum: Drop =
        Int32.MinValue, Int32.MinValue, Int32.MinValue

    let maximum =
        Int32.MaxValue, Int32.MaxValue, Int32.MaxValue

    let add (x1, y1, z1) (x2, y2, z2) : Drop = x1 + x2, y1 + y2, z1 + z2
    let addScalar s (x, y, z) : Drop = x + s, y + s, z + s
    let subtractTo (x1, y1, z1) (x2, y2, z2) : Drop = x1 - x2, y1 - y2, z1 - z2
    let subtract d1 d2 : Drop = subtractTo d2 d1
    let min (x1, y1, z1) (x2, y2, z2) : Drop = min x1 x2, min y1 y2, min z1 z2
    let max (x1, y1, z1) (x2, y2, z2) : Drop = max x1 x2, max y1 y2, max z1 z2

type ProblemInput = Drop []

let parseInput lines : ProblemInput =
    let parser =
        let dropParser =
            pipe3 (pint32 .>> pchar ',') (pint32 .>> pchar ',') pint32 (fun x y z -> (x, y, z))

        sepBy1 dropParser newline |>> Array.ofList

    match run parser lines with
    | Success (result, _, _) -> result
    | Failure (message, _, _) -> failwith message

let part1 input =
    let stopwatch = Stopwatch.StartNew()

    let minDrop, maxDrop =
        input
        |> Array.fold
            (fun (minDrop, maxDrop) drop -> Drop.min minDrop drop, Drop.max maxDrop drop)
            (Drop.maximum, Drop.minimum)

    printfn "minDrop: %A, maxDrop: %A" minDrop maxDrop

    let len1, len2, len3 =
        maxDrop
        |> Drop.subtract minDrop
        |> Drop.addScalar 1

    let boulder =
        Array3D.zeroCreate BoulderSize BoulderSize BoulderSize

    let mutable surface = 0

    printfn "len1: %d, len2: %d, len3: %d" len1 len2 len3

    // Fill boulder with lava
    for i = 0 to input.Length - 1 do
        let x, y, z = input.[i]

        if boulder.[x, y, z] = Lava then
            failwithf "Collision at %A" (x, y, z)
        else
            boulder.[x, y, z] <- Lava
            surface <- surface + 6

            if x > 0 && boulder.[x - 1, y, z] = Lava then
                surface <- surface - 2

            if x < BoulderSize - 1 && boulder.[x + 1, y, z] = Lava then
                surface <- surface - 2

            if y > 0 && boulder.[x, y - 1, z] = Lava then
                surface <- surface - 2

            if y < BoulderSize - 1 && boulder.[x, y + 1, z] = Lava then
                surface <- surface - 2

            if z > 0 && boulder.[x, y, z - 1] = Lava then
                surface <- surface - 2

            if z < BoulderSize - 1 && boulder.[x, y, z + 1] = Lava then
                surface <- surface - 2

    printfn "Elapsed: %A" stopwatch.Elapsed
    printfn "Surface Part1: %d" surface

    stopwatch.Restart()

    // Fill exterior with water
    if boulder.[BoulderSize - 1, BoulderSize - 1, BoulderSize - 1] <> Air then
        failwith $"Expected steam at {BoulderSize - 1},{BoulderSize - 1},{BoulderSize - 1}"

    let queue = Queue<_>()
    let mutable waterCount = 0
    queue.Enqueue(BoulderSize - 1, BoulderSize - 1, BoulderSize - 1)
    boulder.[BoulderSize - 1, BoulderSize - 1, BoulderSize - 1] <- Water
    waterCount <- waterCount + 1

    for a = 0 to BoulderSize - 1 do
        for b = 0 to BoulderSize - 1 do
            if boulder.[a, b, 0] = Air then
                boulder.[a, b, 0] <- Water
                queue.Enqueue(a, b, 0)
                waterCount <- waterCount + 1
            if boulder.[a, b, BoulderSize - 1] = Air then
                boulder.[a, b, BoulderSize - 1] <- Water
                queue.Enqueue(a, b, BoulderSize - 1)
                waterCount <- waterCount + 1
            if boulder.[a, 0, b] = Air then
                boulder.[a, 0, b] <- Water
                queue.Enqueue(a, 0, b)
                waterCount <- waterCount + 1
            if boulder.[a, BoulderSize - 1, b] = Air then
                boulder.[a, BoulderSize - 1, b] <- Water
                queue.Enqueue(a, BoulderSize - 1, b)
                waterCount <- waterCount + 1
            if boulder.[0, a, b] = Air then
                boulder.[0, a, b] <- Water
                queue.Enqueue(0, a, b)
                waterCount <- waterCount + 1
            if boulder.[BoulderSize - 1, a, b] = Air then
                boulder.[BoulderSize - 1, a, b] <- Water
                queue.Enqueue(BoulderSize - 1, a, b)
                waterCount <- waterCount + 1

    while queue.Count > 0 do
        let x, y, z = queue.Dequeue()
        boulder.[x, y, z] <- Water
        // printfn "Water %d at %d,%d,%d" waterCount x y z
        if waterCount > BoulderSize * BoulderSize * BoulderSize then
            failwith "Too much water!!!"

        if x > 0 && boulder.[x - 1, y, z] = Air then
            queue.Enqueue(x - 1, y, z)
            boulder.[x - 1, y, z] <- Water
            waterCount <- waterCount + 1

        if x < BoulderSize - 1 && boulder.[x + 1, y, z] = Air then
            queue.Enqueue(x + 1, y, z)
            boulder.[x + 1, y, z] <- Water
            waterCount <- waterCount + 1

        if y > 0 && boulder.[x, y - 1, z] = Air then
            queue.Enqueue(x, y - 1, z)
            boulder.[x, y - 1, z] <- Water
            waterCount <- waterCount + 1

        if y < BoulderSize - 1 && boulder.[x, y + 1, z] = Air then
            queue.Enqueue(x, y + 1, z)
            boulder.[x, y + 1, z] <- Water
            waterCount <- waterCount + 1

        if z > 0 && boulder.[x, y, z - 1] = Air then
            queue.Enqueue(x, y, z - 1)
            boulder.[x, y, z - 1] <- Water
            waterCount <- waterCount + 1

        if z < BoulderSize - 1 && boulder.[x, y, z + 1] = Air then
            queue.Enqueue(x, y, z + 1)
            boulder.[x, y, z + 1] <- Water
            waterCount <- waterCount + 1

    printfn "Filled exterior with %d water" waterCount

    // Count steam surface
    let mutable steamSurface = 0

    for x = 0 to BoulderSize - 1 do
        for y = 0 to BoulderSize - 1 do
            for z = 0 to BoulderSize - 1 do
                if boulder.[x, y, z] = Air then
                    boulder.[x, y, z] <- Steam
                    steamSurface <- steamSurface + 6

                    if x > 0 && boulder.[x - 1, y, z] = Steam then
                        steamSurface <- steamSurface - 2

                    if x < BoulderSize - 1 && boulder.[x + 1, y, z] = Steam then
                        steamSurface <- steamSurface - 2

                    if y > 0 && boulder.[x, y - 1, z] = Steam then
                        steamSurface <- steamSurface - 2

                    if y < BoulderSize - 1 && boulder.[x, y + 1, z] = Steam then
                        steamSurface <- steamSurface - 2

                    if z > 0 && boulder.[x, y, z - 1] = Steam then
                        steamSurface <- steamSurface - 2

                    if z < BoulderSize - 1 && boulder.[x, y, z + 1] = Steam then
                        steamSurface <- steamSurface - 2

    printfn "Steam Surface Part2: %d" steamSurface
    printfn "Lava Surface Part2: %d" (surface - steamSurface)
    printfn "Elapsed: %A" stopwatch.Elapsed

let execute lines =
    let input = parseInput lines

    // printfn "Problem: %A\n" input
    part1 input

Environment.GetCommandLineArgs().[2]
|> File.ReadAllText
|> execute

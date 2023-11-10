open System
open System.Numerics
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = Instruction []

and Instruction =
    { modeOn: bool; cuboid: Cuboid }

    override this.ToString () =
        let mode = if this.modeOn then "on" else "off"
        sprintf "%s %O" mode this.cuboid

and [<Struct>] Cuboid =
    { min: Position; max: Position }
    member this.Size =
        let x = uint64 (this.max.x - this.min.x + 1)
        let y = uint64 (this.max.y - this.min.y + 1)
        let z = uint64 (this.max.z - this.min.z + 1)
        x * y * z
    member this.MaxSize =
        let x = this.max.x - this.min.x + 1
        let y = this.max.y - this.min.y + 1
        let z = this.max.z - this.min.z + 1
        max x (max y z)
    member this.MaxSizePower =
        let size = this.MaxSize |> uint64
        BitOperations.Log2 (BitOperations.RoundUpToPowerOf2 size)

    member this.XSegment = { minValue = this.min.x; maxValue = this.max.x }
    member this.YSegment = { minValue = this.min.y; maxValue = this.max.y }
    member this.ZSegment = { minValue = this.min.z; maxValue = this.max.z }

    member this.Contains (other: Position) =
        other.x >= this.min.x && other.x <= this.max.x &&
        other.y >= this.min.y && other.y <= this.max.y &&
        other.z >= this.min.z && other.z <= this.max.z

    member this.Contains (other: Cuboid) =
        this.Contains other.min && this.Contains other.max

    member this.Intersects (other: Cuboid) =
        this.XSegment.Intersects other.XSegment &&
        this.YSegment.Intersects other.YSegment &&
        this.ZSegment.Intersects other.ZSegment

    member this.SplitBy (center: Position) =
        let makeFrom sx sy sz =
            match sx, sy, sz with
            | ValueSome x, ValueSome y, ValueSome z -> Cuboid.FromSegments x y z |> ValueSome
            | _ -> ValueNone

        let x1, x2 = this.XSegment.SplitBy center.x
        let y1, y2 = this.YSegment.SplitBy center.y
        let z1, z2 = this.ZSegment.SplitBy center.z

        let xs = [| x1; x2 |]
        let ys = [| y1; y2 |]
        let zs = [| z1; z2 |]

        [|
            for x = 0 to 1 do
                for y = 0 to 1 do
                    for z = 0 to 1 do
                        makeFrom xs.[x] ys.[y] zs.[z]
        |]


    override this.ToString () =
        sprintf "(%O, %O) - %d" this.min this.max this.Size

    static member FromSegments (x: Segment) (y: Segment) (z: Segment) =
        let min = { x = x.minValue; y = y.minValue; z = z.minValue }
        let max = { x = x.maxValue; y = y.maxValue; z = z.maxValue }
        { min = min; max = max }

    static member FromCenter (center: Position) (radius: int) =
        let min = { x = center.x - radius; y = center.y - radius; z = center.z - radius }
        let max = { x = center.x + radius; y = center.y + radius; z = center.z + radius }
        { min = min; max = max }

and [<Struct>] Position =
    { x: int; y: int; z: int }
    member this.Minimum (other: Position) =
        { x = min this.x other.x; y = min this.y other.y; z = min this.z other.z }

    member this.Maximum (other: Position) =
        { x = max this.x other.x; y = max this.y other.y; z = max this.z other.z }

    override this.ToString () =
        sprintf "(%d, %d, %d)" this.x this.y this.z

    static member Create x y z = { x = x; y = y; z = z }
    static member Zero = { x = 0; y = 0; z = 0 }

    static member ( + ) (a: Position, b: Position) =
        { x = a.x + b.x; y = a.y + b.y; z = a.z + b.z }

    static member ( * ) (p: Position, n: int) =
        { x = p.x * n; y = p.y * n; z = p.z * n }

and [<Struct>] Segment =
    { minValue: int; maxValue: int }
    member this.Size = this.maxValue - this.minValue + 1
    member this.SizePower = BitOperations.Log2 (BitOperations.RoundUpToPowerOf2 (uint64 this.Size))

    member this.Contains (value: int) =
        value >= this.minValue && value <= this.maxValue

    member this.Intersects (other: Segment) =
        this.minValue <= other.maxValue && this.maxValue >= other.minValue

    member this.SplitBy (center: int) =
        if center <= this.minValue then
            ValueNone, ValueSome this
        elif center >= this.maxValue then
            ValueSome this, ValueNone
        else
            let left = { minValue = this.minValue; maxValue = center }
            let right = { minValue = center; maxValue = this.maxValue }
            ValueSome left, ValueSome right

and OptimInput = Input

let instructionRegex =
    let num = @"\-?\d+"
    let coord (n: string) = $@"(?<{n}>{num})"
    let x1 = coord "x1"
    let x2 = coord "x2"
    let y1 = coord "y1"
    let y2 = coord "y2"
    let z1 = coord "z1"
    let z2 = coord "z2"
    Regex($@"^(?<mode>on|off) x={x1}..{x2},y={y1}..{y2},z={z1}..{z2}$")

let parseInstruction (line: string) =
    match instructionRegex.Match line with
    | m when m.Success ->
        let modeOn = m.Groups.["mode"].Value = "on"

        let minPos =
            { x = int m.Groups.["x1"].Value
              y = int m.Groups.["y1"].Value
              z = int m.Groups.["z1"].Value }

        let maxPos =
            { x = int m.Groups.["x2"].Value
              y = int m.Groups.["y2"].Value
              z = int m.Groups.["z2"].Value }

        assert (minPos.x <= maxPos.x)
        assert (minPos.y <= maxPos.y)
        assert (minPos.z <= maxPos.z)

        let cuboid = { min = minPos; max = maxPos }

        { modeOn = modeOn; cuboid = cuboid }
    | _ -> failwithf "Invalid instruction: %s" line

let parseInput (text: string []) = text |> Array.map parseInstruction

let optimizeInput (input: Input) = input

let step1Naive (input: OptimInput) =
    let array = Array3D.zeroCreate 101 101 101

    let mainCuboid = { min = { x = -50; y = -50; z = -50 }; max = { x = 50; y = 50; z = 50 } }

    let inline isInside (cuboid: Cuboid) = mainCuboid.Contains cuboid

    for i in input do
        if isInside i.cuboid then
            for x = i.cuboid.min.x to i.cuboid.max.x do
                for y = i.cuboid.min.y to i.cuboid.max.y do
                    for z = i.cuboid.min.z to i.cuboid.max.z do
                        array.[x+50, y+50, z+50] <- i.modeOn

    let mutable onCount = 0
    for x = 0 to 100 do
        for y = 0 to 100 do
            for z = 0 to 100 do
                if array.[x, y, z] then
                    onCount <- onCount + 1

    onCount


let Directions = [|
    for x = 0 to 1 do
        for y = 0 to 1 do
            for z = 0 to 1 do
                Position.Create (x * 2 - 1) (y * 2 - 1) (z * 2 - 1)
|]

type ReactorNode =
    { size: int
      center: Position
      cuboid: Cuboid
      content: ReactorContent }

and ReactorContent =
    | Leaf of modeOn: bool
    | Branch of nodes: ReactorNode [] // 8 nodes representing 8 octants

module ReactorNode =
    let MaxInputSize = 18

    let powerSize (power: int) = 1 <<< power
    let powerCubed (power: int) =
        let size = powerSize power |> uint64
        size * size * size

    let createLeaf size center modeOn =
        let radius = powerSize (size - 1)
        let cuboid = Cuboid.FromCenter center radius
        { ReactorNode.size = size; center = center; cuboid = cuboid; content = Leaf modeOn }

    let empty = createLeaf MaxInputSize Position.Zero false

    let rec splitNode (node: ReactorNode) =
        match node.content with
        | Leaf modeOn ->
            if node.size <= 0 then
                failwithf "Node %O is too small to split" node
            else
                let size = node.size - 1
                let radius = powerSize (size - 1)
                let nodes =
                    Directions
                    |> Array.map (fun d ->
                        let center = node.center + d * radius
                        let cuboid = Cuboid.FromCenter center radius
                        { ReactorNode.size = size; center = center; cuboid = cuboid; content = Leaf modeOn })
                { node with content = Branch nodes }
        | _ -> node

    let count (modeOn: bool) (node: ReactorNode) =
        let rec loop (node: ReactorNode) =
            match node.content with
            | Leaf mode when mode = modeOn -> powerCubed node.size
            | Leaf _ -> 0UL
            | Branch nodes ->
                let mutable result = 0UL
                for n in nodes do
                    result <- result + loop n
                result

        loop node

    let insert (instruction: Instruction) (node: ReactorNode) =
        let rec loop (instruction: Instruction) (node: ReactorNode) =
            if node.size = 0 || instruction.cuboid = node.cuboid then
                { node with content = Leaf instruction.modeOn }
            elif node.cuboid.Contains instruction.cuboid |> not then
                failwithf "Instruction %A is outside of reactor node %O" instruction node.cuboid
            else
                match node.content with
                | Leaf modeOn when instruction.modeOn = modeOn ->
                    node

                | Leaf _ ->
                    let node = splitNode node
                    loop instruction node

                | Branch nodes ->
                    let splitCuboids = instruction.cuboid.SplitBy node.center // 8 optional cuboids
                    let nodes =
                        Seq.zip nodes splitCuboids
                        |> Seq.map (fun (subNode, subCuboid) ->
                            match subCuboid with
                            | ValueNone -> subNode
                            | ValueSome cuboid ->
                                let instruction' = { instruction with cuboid = cuboid }
                                loop instruction' subNode)
                        |> Seq.toArray
                    { node with content = Branch nodes }

        loop instruction node


let step1 (input: OptimInput) =
    let mainCuboid = { min = { x = -50; y = -50; z = -50 }; max = { x = 50; y = 50; z = 50 } }

    let inline isInside (cuboid: Cuboid) = mainCuboid.Contains cuboid

    let mutable reactor =
        input
        |> Array.fold
            (fun reactor instruction ->
                if isInside instruction.cuboid then
                    ReactorNode.insert instruction reactor
                else
                    reactor)
            ReactorNode.empty

    reactor |> ReactorNode.count true

let step2 (input: OptimInput) =
    2


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let input = optimizeInput input
    // printfn "OptimInput:\n%O" input

    let n = input.Length
    let time = Time.measureColdN

    (fun () -> step1Naive input)
    |> time n
    |> printfn "***** STEP1 Naive:\n%O"

    (fun () -> step1 input)
    |> time n
    |> printfn "***** STEP1:\n%O"

    (fun () -> step2 input)
    |> time n
    |> printfn "***** STEP2:\n%A"


main ()

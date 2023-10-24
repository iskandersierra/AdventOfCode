open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = string []

and OptimInput = Packet []

and Packet =
    { version: int
      packetType: int
      content: PacketContent }

and PacketContent =
    | Literal of int64
    | Operator of OperatorType * Packet []

and OperatorType =
    | SumOperator
    | ProductOperator
    | MinimumOperator
    | MaximumOperator
    | GreaterThanOperator
    | LessThanOperator
    | EqualToOperator

and Output1 =
    { results: OutputResult1 [] }
    override this.ToString() =
        let sb = StringBuilder()

        for result in this.results do
            sb.AppendLine(result.ToString()) |> ignore

        sb.ToString()

and OutputResult1 =
    { result: int64
      packet: Packet }
    override this.ToString() =
        let sb = StringBuilder()

        // sb.AppendLine($"  Packet: %A{this.packet}") |> ignore

        sb.AppendLine($"Result: {this.result}") |> ignore

        sb.ToString()

let parseInput (text: string []) = text

let hexToBinary =
    let hexIndex =
        "0123456789ABCDEF"
        |> Seq.mapi (fun i c -> c, i)
        |> dict

    let binaries =
        [ 0 .. 15 ]
        |> Seq.map (fun i -> Convert.ToString(i, 2).PadLeft(4, '0'))
        |> Seq.toArray

    fun (hex: string) ->
        let len = hex.Length
        let sb = StringBuilder(len * 4)

        for i = 0 to len - 1 do
            let c = hex.[i]
            let index = hexIndex.[c]
            let bin = binaries.[index]
            sb.Append(bin) |> ignore

        sb.ToString()

let IsLastGroup = 0
let LiteralValue = 4

let operators =
    [ 0, SumOperator
      1, ProductOperator
      2, MinimumOperator
      3, MaximumOperator
      5, GreaterThanOperator
      6, LessThanOperator
      7, EqualToOperator ]
    |> dict

type Parser =
    { binary: string
      index: int }
    static member FromBinary(binary: string) = { binary = binary; index = 0 }

let parseBit parser =
    let c = parser.binary.[parser.index]
    let parser = { parser with index = parser.index + 1 }
    let value = if c = '0' then 0 else 1
    value, parser

let parseNumber bits parser =
    let rec loop remainingBits value parser =
        if remainingBits = 0 then
            value, parser
        else
            let bit, parser = parseBit parser
            let value = (value <<< 1) + int64 bit
            loop (remainingBits - 1) value parser

    loop bits 0L parser

let parseVersion parser =
    parseNumber 3 parser
    |> fun (version, parser) -> int version, parser

let parseType parser =
    parseNumber 3 parser
    |> fun (packetType, parser) -> int packetType, parser

let parseLiteralGroups parser =
    let rec loop value parser =
        let isLast, parser = parseBit parser
        let bits, parser = parseNumber 4 parser
        let value = (value <<< 4) + bits

        if isLast = IsLastGroup then
            value, parser
        else
            loop value parser

    loop 0L parser

let parseLiteralPacket parser =
    let value, parser = parseLiteralGroups parser
    Literal value, parser

let parseOperatorHeader parser =
    let sizeType, parser = parseNumber 1 parser

    let keepParsing, parser =
        if sizeType = 0 then
            let size, parser = parseNumber 15 parser

            let keepParsing _ newParser =
                newParser.index - parser.index < int size

            keepParsing, parser

        else
            let size, parser = parseNumber 11 parser
            let keepParsing (packets: ResizeArray<_>) _ = packets.Count < int size
            keepParsing, parser

    keepParsing, parser

let rec parseOperatorPacket packetType parser =
    let keepParsing, parser = parseOperatorHeader parser
    let operatorType = operators.[packetType]
    let packets = ResizeArray()
    let mutable parser = parser

    while keepParsing packets parser do
        let packet, parser' = parsePacket parser
        packets.Add(packet) |> ignore
        parser <- parser'

    let packets = packets.ToArray()
    Operator(operatorType, packets), parser

and parsePacket parser =
    let version, parser = parseVersion parser
    let packetType, parser = parseType parser

    let content, parser =
        if packetType = LiteralValue then
            parseLiteralPacket parser
        else
            parseOperatorPacket packetType parser

    { version = version
      packetType = packetType
      content = content },
    parser

let rec addVersions packet =
    match packet.content with
    | Literal _ -> packet.version
    | Operator (_, packets) ->
        let versions = packets |> Seq.sumBy addVersions
        packet.version + versions

let rec computePacket packet =
    match packet.content with
    | Literal value -> value
    | Operator (operatorType, packets) ->
        let values = packets |> Array.map computePacket

        match operatorType with
        | SumOperator -> values |> Array.sum
        | ProductOperator -> values |> Array.reduce (*)
        | MinimumOperator -> values |> Array.min
        | MaximumOperator -> values |> Array.max
        | GreaterThanOperator -> if values.[0] > values.[1] then 1 else 0
        | LessThanOperator -> if values.[0] < values.[1] then 1 else 0
        | EqualToOperator -> if values.[0] = values.[1] then 1 else 0

let step1 (input: Input) =
    input
    |> Array.map (
        hexToBinary
        >> Parser.FromBinary
        >> parsePacket
        >> fst
    )
    |> Array.map (fun packet ->
        { result = addVersions packet
          packet = packet })
    |> fun results -> { results = results }

let step2 (input: Input) =
    input
    |> Array.map (
        hexToBinary
        >> Parser.FromBinary
        >> parsePacket
        >> fst
    )
    |> Array.map (fun packet ->
        { result = computePacket packet
          packet = packet })
    |> fun results -> { results = results }


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input

    let n = input |> Seq.sumBy (fun x -> x.Length)

    let result1 = Time.measureN n (fun () -> step1 input)

    printfn "***** STEP1:\n%O" result1

    let result2 = Time.measureN n (fun () -> step2 input)

    printfn "***** STEP2:\n%O" result2


main ()

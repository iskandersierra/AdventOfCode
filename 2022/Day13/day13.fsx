#r "nuget: FParsec, 1.1.1"

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open FParsec

#nowarn "0025"

type Packet =
    | ValuePacket of int
    | ListPacket of Packet list
    override this.ToString() =
        match this with
        | ValuePacket v -> sprintf "%d" v
        | ListPacket l -> sprintf "[%s]" (String.Join(",", l |> List.map string))

let parseInput (lines: string) =
    let packetParser, packetParserRef =
        createParserForwardedToRef<Packet, unit> ()

    let valueParser: Parser<Packet, unit> = pint32 |>> ValuePacket

    let listParser: Parser<Packet, unit> =
        between (skipChar '[') (skipChar ']') (sepBy packetParser (skipChar ','))
        |>> ListPacket

    packetParserRef.Value <- choice [ valueParser; listParser ]

    let packetPairParser =
        pipe2 packetParser (newline >>. packetParser) (fun a b -> a, b)

    let packetListParser =
        sepBy1 packetPairParser (newline >>. newline)

    run packetListParser (lines)

let rec comparePackets packet1 packet2 =
    match packet1, packet2 with
    | ValuePacket v1, ValuePacket v2 -> v1.CompareTo(v2)
    | ValuePacket _, ListPacket _ -> comparePackets (ListPacket [ packet1 ]) packet2
    | ListPacket _, ValuePacket _ -> comparePackets packet1 (ListPacket [ packet2 ])
    | ListPacket l1, ListPacket l2 -> comparePacketLists l1 l2

and comparePacketLists l1 l2 =
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1 :: t1, h2 :: t2 ->
        let c = comparePackets h1 h2

        if c = 0 then
            comparePacketLists t1 t2
        else
            c

let part1 lines =
    match parseInput lines with
    | Failure (error, _, _) -> error
    | Success (result, _, _) ->
        result
        |> Seq.mapi (fun index (a, b) -> index + 1, comparePackets a b)
        |> Seq.filter (fun (_, comparison) -> comparison < 0)
        |> Seq.map fst
        |> Seq.sum
        |> sprintf "%d"

let part2 lines =
    match parseInput lines with
    | Failure (error, _, _) -> error
    | Success (result, _, _) ->
        let packet2 =
            ListPacket [ ListPacket [ ValuePacket 2 ] ]
        let packet6 =
            ListPacket [ ListPacket [ ValuePacket 6 ] ]

        result
        |> Seq.collect (fun (a, b) -> [ a; b ])
        |> Seq.append [ packet2; packet6]
        |> Seq.sortWith comparePackets
        |> Seq.mapi (fun index packet -> index + 1, packet)
        |> Seq.filter (fun (_, packet) -> packet = packet2 || packet = packet6)
        |> Seq.map fst
        |> Seq.fold (*) 1
        |> sprintf "%d"

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllText fileName
|> if part = "1" then part1 else part2
|> printfn "%s"

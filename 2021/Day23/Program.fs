open System
open System.Numerics
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

let HallwaySize = 11
let RoomSize = 2
let RoomPositions = [| 2; 4; 6; 8 |]

let inline getAmphipodByte (ch: char) =
    if ch >= 'A' && ch <= 'D' then
        byte ch - byte 'A' + 1uy
    else
        failwithf "Invalid amphipod char: %c" ch

let inline getAmphipodChar (value: byte) =
    if value = 0uy then '.' else char (value + byte 'A' - 1uy)

let inline getAmphipodRoomIndex (value: byte) =
    int value - 1

let inline isRoomPosition (pos: int) =
    RoomPositions |> Array.contains pos

let AmphipodEnergy = [| 0; 1; 10; 100; 1000 |]

type Input =
    { data: byte [] }
    static member inline TotalSize = HallwaySize + RoomPositions.Length * RoomSize
    static member inline Create() = { data = Array.zeroCreate Input.TotalSize }

    member inline this.Clone() = { data = Array.copy this.data }

    member inline this.GetRoomPos (roomIndex: int) (roomPos: int) =
        HallwaySize + roomIndex * RoomSize + roomPos

    member inline this.SetAtRoom (roomIndex: int) (roomPos: int) (value: byte) =
        let pos = this.GetRoomPos roomIndex roomPos
        this.data.[pos] <- value

    member inline this.GetAtRoom (roomIndex: int) (roomPos: int) =
        let pos = this.GetRoomPos roomIndex roomPos
        this.data.[pos]

    member this.IsRoomFree (roomIndex: int) (expectedValue: byte) =
        let mutable isFree = true
        let mutable index = 0
        while index < RoomSize && isFree do
            let value = this.GetAtRoom roomIndex index
            isFree <- value = 0uy || value = expectedValue
            index <- index + 1
        isFree

    member this.FindHallwayToRoomForwardPath (fromHallwayIndex: int) (toRoomIndex: int) =
        let amphipodValue = this.data.[fromHallwayIndex]
        let roomHallwayPos = RoomPositions.[toRoomIndex]
        ()

    member this.GetForwardMoves() =
        // - Amphipods will never stop on the space immediately outside any room.
        //   They can move into that space so long as they immediately continue moving.
        //   (Specifically, this refers to the four open spaces in the hallway
        //   that are directly above an amphipod starting position.)
        // - Amphipods will never move from the hallway into a room unless that
        //   room is their destination room and that room contains no amphipods
        //   which do not also have that room as their own destination.
        //   If an amphipod's starting room is not its destination room, it can
        //   stay in that room until it leaves the room. (For example, an Amber
        //   amphipod will not move from the hallway into the right three rooms,
        //   and will only move into the leftmost room if that room is empty or
        //   if it only contains other Amber amphipods.)
        // - Once an amphipod stops moving in the hallway, it will stay in that
        //   spot until it can move into a room. (That is, once any amphipod
        //   starts moving, any other amphipods currently in the hallway are
        //   locked in place and will not move again until they can move fully
        //   into a room.)
        seq {
            // Hallway - Try to move any amphipod from the hallway into its destination room
            for index = 0 to HallwaySize - 1 do
                let value = this.data.[index]
                if value > 0uy then
                    let targetRoomIndex = getAmphipodRoomIndex value
                    if this.IsRoomFree targetRoomIndex value then
                        match this.FindHallwayToRoomForwardPath index targetRoomIndex with
                        | Some path -> yield path
                        | None -> ()
        }

    override this.ToString () =
        let sb = StringBuilder()

        (*
            #############
            #...........#
            ###B#C#B#D###
              #A#D#C#A#
              #########
        *)

        let inline appendValue (value: byte) =
            if value = 0uy then '.' else getAmphipodChar value
            |> sb.Append
            |> ignore

        // #############
        String('#', HallwaySize + 2) |> sb.AppendLine |> ignore

        // #...........#
        sb.Append ("#") |> ignore
        for i = 0 to HallwaySize - 1 do
            appendValue this.data.[i]
        sb.AppendLine ("#") |> ignore

        // ###B#C#B#D### or   #A#D#C#A#
        for roomPos = 0 to RoomSize - 1 do
            let borderChar = if roomPos = 0 then '#' else ' '

            String(borderChar, RoomPositions.[0]) |> sb.Append |> ignore

            for roomIndex = 0 to RoomPositions.Length - 1 do
                sb.Append ("#") |> ignore
                let value = this.data.[HallwaySize + roomIndex * RoomSize + roomPos]
                appendValue value

            sb.Append ("#") |> ignore

            String(borderChar, HallwaySize - RoomPositions.[RoomPositions.Length - 1] - 1) |> sb.AppendLine |> ignore

        //   #########

        String(' ', RoomPositions.[0]) |> sb.Append |> ignore
        String('#', RoomPositions.[RoomPositions.Length - 1] - RoomPositions.[0] + 3) |> sb.AppendLine |> ignore

        sb.ToString()

and OptimInput = Input

type InputEqualityComparer() =
    static member Default = new InputEqualityComparer()

    interface IEqualityComparer<Input> with
        member this.Equals(x: Input, y: Input) = x.data = y.data

        member this.GetHashCode(x: Input) =
            let hash = HashCode()
            hash.Add x.data.Length

            for i = 0 to x.data.Length - 1 do
                hash.Add x.data.[i]

            hash.ToHashCode()

let parseInput (text: string []) =
    let input = Input.Create()

    for roomPos = 0 to RoomSize - 1 do
        let line = text.[2 + roomPos]

        for roomIndex = 0 to RoomPositions.Length - 1 do
            let amphipodChar = line.[RoomPositions.[roomIndex] + 1]
            let amphipodIndex = getAmphipodByte amphipodChar
            input.SetAtRoom roomIndex roomPos amphipodIndex

    input

let optimizeInput (input: Input) = input

let step1 (input: OptimInput) = 1

let step2 (input: OptimInput) = 2


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let input = optimizeInput input
    printfn "OptimInput:\n%O" input

    let n = HallwaySize + RoomPositions.Length * RoomSize
    let time = Time.measureColdN

    (fun () -> step1 input)
    |> time n
    |> printfn "***** STEP1:\n%O"

    (fun () -> step2 input)
    |> time n
    |> printfn "***** STEP2:\n%A"


main ()

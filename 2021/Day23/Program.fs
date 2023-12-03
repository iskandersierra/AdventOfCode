open System
open System.Numerics
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open AocUtils.Search

let HallwaySize = 11
let RoomSize = 2
let RoomPositions = [| 2; 4; 6; 8 |]

let inline getAmphipodByte (ch: char) =
    if ch >= 'A' && ch <= 'D' then
        byte ch - byte 'A' + 1uy
    else
        failwithf "Invalid amphipod char: %c" ch

let inline getAmphipodChar (value: byte) =
    if value = 0uy then
        '.'
    else
        char (value + byte 'A' - 1uy)

let inline getAmphipodRoomIndex (value: byte) = int value - 1

let inline isRoomPosition (pos: int) = RoomPositions |> Array.contains pos

let AmphipodEnergy = [| 0; 1; 10; 100; 1000 |]

type Input = byte []

type Decision =
    | MoveFromHallwayToRoom of amphipod: byte * hallwayPos: int * roomIndex: int * cost: int
    | MoveFromRoomToHallway of amphipod: byte * roomIndex: int * hallwayPos: int * cost: int

module Input =
    let totalSize =
        HallwaySize + RoomPositions.Length * RoomSize

    let inline create () = Array.zeroCreate totalSize
    let inline clone (input: Input) = Array.copy input

    let inline getRoomPos (roomIndex: int) (roomPos: int) =
        HallwaySize + roomIndex * RoomSize + roomPos

    let inline getAtHallway (hallwayPos: int) (input: Input) = Array.get input hallwayPos

    let inline setAtHallway (hallwayPos: int) (value: byte) (input: Input) = Array.set input hallwayPos value

    let inline setAtRoom (roomIndex: int) (roomPos: int) (value: byte) (input: Input) =
        let pos = getRoomPos roomIndex roomPos
        Array.set input pos value

    let inline getAtRoom (roomIndex: int) (roomPos: int) (input: Input) =
        let pos = getRoomPos roomIndex roomPos
        Array.get input pos

    let finalBoard =
        let board = create ()

        for roomIndex = 0 to RoomPositions.Length - 1 do
            let amphipodByte = byte (roomIndex + 1)

            for roomPos = 0 to RoomSize - 1 do
                setAtRoom roomIndex roomPos amphipodByte board

        board

    let isRoomFree (roomIndex: int) (expectedValue: byte) (input: Input) =
        let mutable isFree = true
        let mutable index = 0

        while index < RoomSize && isFree do
            let value = getAtRoom roomIndex index input
            isFree <- value = 0uy || value = expectedValue
            index <- index + 1

        isFree

    let getEmptyRoomPos (roomIndex: int) (input: Input) : int voption =
        let mutable index = RoomSize - 1
        let mutable found = false

        while index >= 0 && not found do
            let value = getAtRoom roomIndex index input
            found <- value = 0uy
            if not found then index <- index - 1

        if found then
            ValueSome index
        else
            ValueNone

    let canAcceptAmphipodToRoom (roomIndex: int) (amphipodByte: byte) (input: Input) =
        let mutable canAccept = true
        let mutable index = 0

        while index < RoomSize && canAccept do
            let value = getAtRoom roomIndex index input
            canAccept <- value = 0uy || value = amphipodByte
            index <- index + 1

        canAccept

    let isHallwayEmptyAfterStart (hallwayPos: int) (roomHallwayPos: int) (input: Input) =
        if hallwayPos < roomHallwayPos then
            let mutable isFree = true
            let mutable index = hallwayPos + 1

            while index < roomHallwayPos && isFree do
                isFree <- getAtHallway index input = 0uy
                index <- index + 1

            isFree
        else
            let mutable isFree = true
            let mutable index = hallwayPos - 1

            while index > roomHallwayPos && isFree do
                isFree <- getAtHallway index input = 0uy
                index <- index - 1

            isFree

    let findHallwayToRoomForwardDecisions (fromHallwayIndex: int) (toRoomIndex: int) (input: Input) =
        seq {
            let amphipodByte = getAtHallway fromHallwayIndex input
            let roomHallwayPos = RoomPositions.[toRoomIndex]

            if canAcceptAmphipodToRoom toRoomIndex amphipodByte input
               && isHallwayEmptyAfterStart fromHallwayIndex roomHallwayPos input then
                match getEmptyRoomPos toRoomIndex input with
                | ValueSome roomPos ->
                    let newInput = clone input
                    setAtHallway fromHallwayIndex 0uy newInput
                    setAtRoom toRoomIndex roomPos amphipodByte newInput

                    let stepCount =
                        abs (fromHallwayIndex - roomHallwayPos)
                        + (roomPos + 1)

                    let cost =
                        AmphipodEnergy.[int amphipodByte] * stepCount

                    let decision =
                        MoveFromHallwayToRoom(amphipodByte, fromHallwayIndex, toRoomIndex, cost)

                    yield { item = newInput; decision = decision }

                | ValueNone -> ()
            else
                ()
        }

    let getAmphipodPosToMove (roomIndex: int) (input: Input) =
        let amphipodByte = byte (roomIndex + 1)
        let first = getAtRoom roomIndex 0 input
        let second = getAtRoom roomIndex 1 input

        match first, second with
        | f, _ when f <> amphipodByte -> ValueSome 0 // The first amphipod is of a different type
        | 0uy, s when s <> amphipodByte -> ValueSome 1 // The second amphipod is of a different type and the first is free
        | f, s when f = amphipodByte && s <> amphipodByte -> ValueSome 0 // The first amphipod is of the correct type, but the second is not. Still move the first one out to free the second one
        | _ -> ValueNone // Nothing to move out

    let findRoomToHallwayForwardDecisions (roomIndex: int) (input: Input) =
        seq {
            match getAmphipodPosToMove roomIndex input with
            | ValueNone -> ()
            | ValueSome roomPos ->
                let amphipodByte = getAtRoom roomIndex roomPos input
                // find left
                let mutable index = RoomPositions.[roomIndex] - 1
                let mutable keepSearching = true

                while index >= 0 && keepSearching do
                    let value = getAtHallway index input
                    keepSearching <- value = 0uy

                    if keepSearching && not(isRoomPosition index) then
                        let newInput = clone input
                        setAtRoom roomIndex roomPos 0uy newInput
                        setAtHallway index amphipodByte newInput

                        let stepCount =
                            RoomPositions.[roomIndex] - index + (roomPos + 1)

                        let cost =
                            AmphipodEnergy.[int amphipodByte] * stepCount

                        let decision =
                            MoveFromRoomToHallway(amphipodByte, roomIndex, index, cost)

                        yield { item = newInput; decision = decision }

                    index <- index - 1

                // find right
                let mutable index = RoomPositions.[roomIndex] + 1
                let mutable keepSearching = true

                while index < HallwaySize && keepSearching do
                    let value = getAtHallway index input
                    keepSearching <- value = 0uy

                    if keepSearching && not(isRoomPosition index) then
                        let newInput = clone input
                        setAtRoom roomIndex roomPos 0uy newInput
                        setAtHallway index amphipodByte newInput

                        let stepCount =
                            index - RoomPositions.[roomIndex] + (roomPos + 1)

                        let cost =
                            AmphipodEnergy.[int amphipodByte] * stepCount

                        let decision =
                            MoveFromRoomToHallway(amphipodByte, roomIndex, index, cost)

                        yield { item = newInput; decision = decision }

                    index <- index + 1
        }

    let getForwardMoves (input: Input) =
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
                let value = getAtHallway index input

                if value > 0uy then
                    let targetRoomIndex = getAmphipodRoomIndex value

                    if isRoomFree targetRoomIndex value input then
                        yield! findHallwayToRoomForwardDecisions index targetRoomIndex input

            // Rooms - Try to move any amphipod from a room into the hallway
            for roomIndex = 0 to RoomPositions.Length - 1 do
                yield! findRoomToHallwayForwardDecisions roomIndex input
        }

    let toString (input: Input) =
        let sb = StringBuilder()

        (*
            #############
            #...........#
            ###B#C#B#D###
              #A#D#C#A#
              #########
        *)

        let inline appendValue (value: byte) =
            if value = 0uy then
                '.'
            else
                getAmphipodChar value
            |> sb.Append
            |> ignore

        // #############
        String('#', HallwaySize + 2)
        |> sb.AppendLine
        |> ignore

        // #...........#
        sb.Append("#") |> ignore

        for i = 0 to HallwaySize - 1 do
            appendValue input.[i]

        sb.AppendLine("#") |> ignore

        // ###B#C#B#D### or   #A#D#C#A#
        for roomPos = 0 to RoomSize - 1 do
            let borderChar = if roomPos = 0 then '#' else ' '

            String(borderChar, RoomPositions.[0])
            |> sb.Append
            |> ignore

            for roomIndex = 0 to RoomPositions.Length - 1 do
                sb.Append("#") |> ignore

                let value =
                    input.[HallwaySize + roomIndex * RoomSize + roomPos]

                appendValue value

            sb.Append("#") |> ignore

            String(
                borderChar,
                HallwaySize
                - RoomPositions.[RoomPositions.Length - 1]
                - 1
            )
            |> sb.AppendLine
            |> ignore

        //   #########

        String(' ', RoomPositions.[0])
        |> sb.Append
        |> ignore

        String(
            '#',
            RoomPositions.[RoomPositions.Length - 1]
            - RoomPositions.[0]
            + 3
        )
        |> sb.AppendLine
        |> ignore

        sb.ToString()

type OptimInput = Input

type InputEqualityComparer() =
    static member Default: IEqualityComparer<Input> = new InputEqualityComparer()

    interface IEqualityComparer<Input> with
        member this.Equals(x: Input, y: Input) = x = y

        member this.GetHashCode(input: Input) =
            let hash = HashCode()
            hash.Add input.Length

            for i = 0 to input.Length - 1 do
                hash.Add input.[i]

            hash.ToHashCode()

let parseInput (text: string []) =
    let input = Input.create ()

    for roomPos = 0 to RoomSize - 1 do
        let line = text.[2 + roomPos]

        for roomIndex = 0 to RoomPositions.Length - 1 do
            let amphipodChar = line.[RoomPositions.[roomIndex] + 1]
            let amphipodByte = getAmphipodByte amphipodChar

            input
            |> Input.setAtRoom roomIndex roomPos amphipodByte

    input

let searchDescription (sourceItems: Input seq) =
    { new ISearchDescription<Input, Decision> with
        member this.GetSourceItems() = sourceItems

        member this.IsSolutionItem item =
            InputEqualityComparer.Default.Equals(item, Input.finalBoard)

        member this.GetDecisions item = Input.getForwardMoves item }

let prioritySearch (sourceItems: Input seq) =
    let search = searchDescription sourceItems

    { new IPrioritySearch<Input, Decision, int> with
        member this.Search = search
        member this.Cost = CostDescription.int32
        member this.ItemComparer = InputEqualityComparer.Default

        member this.GetItemCost item = 0

        member this.GetDecisionCost decision =
            match decision with
            | MoveFromHallwayToRoom (_, _, _, cost)
            | MoveFromRoomToHallway (_, _, _, cost) -> cost }

let optimizeInput (input: Input) = input

let printPath (path: SearchPath<Input, Decision, int>) =
    let mutable step = 1

    for item in path do
        match item with
        | FirstStep (input, cost) -> printfn "Step %d: %d" step cost
        // printfn "%s" (Input.toString input)
        | NextStep (input, decision, cost) ->
            printfn "Step %d: %d" step cost

            match decision with
            | MoveFromHallwayToRoom (amphipod, hallwayPos, roomIndex, cost) ->
                printfn
                    " - Move amphipod %c from hallway pos %d to room %d with cost %d"
                    (getAmphipodChar amphipod)
                    hallwayPos
                    roomIndex
                    cost
            // printfn "%s" (Input.toString input)
            | MoveFromRoomToHallway (amphipod, roomIndex, hallwayPos, cost) ->
                printfn
                    " - Move amphipod %c from room %d to hallway pos %d with cost %d"
                    (getAmphipodChar amphipod)
                    roomIndex
                    hallwayPos
                    cost

        step <- step + 1

let step1 (input: OptimInput) =
    let sourceItems = seq { input }
    let search = prioritySearch sourceItems

    search
    |> PrioritySearch.debugFind
        {
            yieldNode = fun node -> node.isSolution
            continueAfter = fun node -> not node.isSolution
            repeating = ignore
            enqueuing = fun (node, cost) ->
                ()
            dequeuing = ignore
        }
    |> Seq.tryHead
    |> function
        | None -> failwith "No solution found"
        | Some node ->
            let path = PrioritySearch.buildSearchPath node
            printPath path

    //match PrioritySearch.tryFindBest search with
    //| None -> failwith "No solution found"
    //| Some path ->
    //    printPath path

    //    path
    //    |> List.last
    //    |> function
    //        | FirstStep (_, cost) -> cost
    //        | NextStep (_, _, cost) -> cost

let step2 (input: OptimInput) = 2


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let input = optimizeInput input
    printfn "OptimInput:\n%O" (Input.toString input)

    let n =
        HallwaySize + RoomPositions.Length * RoomSize

    let time = Time.measureColdN

    (fun () -> step1 input)
    |> time n
    |> printfn "***** STEP1:\n%O"

    (fun () -> step2 input)
    |> time n
    |> printfn "***** STEP2:\n%A"


main ()

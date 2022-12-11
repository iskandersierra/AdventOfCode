open System
open System.IO
open System.Text
open System.Text.RegularExpressions

#nowarn "0025"

type Operation = Operation of Operand * Operator * Operand

and Operand =
    | Old
    | Value of int64

and Operator =
    | Add
    | Multiply
    | Subtract
    | Divide

type MonkeyNote =
    { index: int
      startingItems: int64 list
      operation: Operation
      testDivisibleBy: int64
      ifTrue: int
      ifFalse: int
      inspectedItems: int64 }

type MonkeyNotes = Map<int, MonkeyNote>

type MonkeyInspection = { item: int64; newIndex: int }

let NotesRegex =
    Regex(
        @"Monkey (?<index>\d+):\s+Starting items:\s*(?<item>\d+)(\s*,\s*(?<item>\d+))*\s+Operation:\s*new\s*=\s*(?<operandl>old|\d+)\s*(?<operator>[\+\-\*\/])\s*(?<operandr>old|\d+)\s+Test: divisible by (?<divisible>\d+)\s*If true: throw to monkey (?<ifTrue>\d+)\s*If false: throw to monkey (?<ifFalse>\d+)\s*",
        RegexOptions.Compiled
        ||| RegexOptions.IgnoreCase
        ||| RegexOptions.ExplicitCapture
    )

let takeNotes lines : MonkeyNotes =
    NotesRegex.Matches(lines)
    |> Seq.map (fun m ->
        let index = int m.Groups.["index"].Value

        let startingItems =
            m.Groups.["item"].Captures
            |> Seq.map (fun c -> int64 c.Value)
            |> List.ofSeq

        let operandl =
            if m.Groups.["operandl"].Value = "old" then
                Old
            else
                Value(int m.Groups.["operandl"].Value)

        let operator =
            match m.Groups.["operator"].Value with
            | "+" -> Add
            | "-" -> Subtract
            | "*" -> Multiply
            | "/" -> Divide
            | _ -> failwith "Invalid operator"

        let operandr =
            if m.Groups.["operandr"].Value = "old" then
                Old
            else
                Value(int m.Groups.["operandr"].Value)

        let operation = Operation(operandl, operator, operandr)
        let testDivisibleBy = int m.Groups.["divisible"].Value
        let ifTrue = int m.Groups.["ifTrue"].Value
        let ifFalse = int m.Groups.["ifFalse"].Value

        index,
        { index = index
          startingItems = startingItems
          operation = operation
          testDivisibleBy = testDivisibleBy
          ifTrue = ifTrue
          ifFalse = ifFalse
          inspectedItems = 0 })
    |> Map.ofSeq

let printNote note =
    sprintf
        "Monkey %d [%3d]: %s"
        note.index
        note.inspectedItems
        (note.startingItems
         |> Seq.map string
         |> String.concat ", ")

let printNotes (notes: MonkeyNotes) =
    let sb = StringBuilder()

    notes
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map snd
    |> Seq.iter (fun note -> sb.AppendLine(printNote note) |> ignore)

    sb.ToString()

let operandValue oldValue operand =
    match operand with
    | Old -> oldValue
    | Value v -> v

let performOperation value (Operation (operandL, operator, operandR)) =
    let operandLValue = operandValue value operandL
    let operandRValue = operandValue value operandR

    match operator with
    | Add -> operandLValue + operandRValue
    | Subtract -> operandLValue - operandRValue
    | Multiply -> operandLValue * operandRValue
    | Divide -> operandLValue / operandRValue

let printOperand isRight operand =
    match operand with
    | Old ->
        if isRight then
            "itself"
        else
            "Worry level"
    | Value v -> string v

let printOperation (Operation (operandL, operator, operandR)) =
    let operandLValue = printOperand false operandL
    let operandRValue = printOperand true operandR

    match operator with
    | Add -> sprintf "%s increases by %s" operandLValue operandRValue
    | Subtract -> sprintf "%s decreases by %s" operandLValue operandRValue
    | Multiply -> sprintf "%s is multiplied by %s" operandLValue operandRValue
    | Divide -> sprintf "%s is divided by %s" operandLValue operandRValue

let inspectItem divideWorry note item =
    // printfn "  Monkey inspects an item with a worry level of %d." item
    let item = performOperation item note.operation
    // printfn "    %s to %d." (printOperation note.operation) item
    let item = if divideWorry then item / 3L else item
    // printfn "    Monkey gets bored with item. Worry level is divided by 3 to %d." item

    let newIndex =
        if item % note.testDivisibleBy = 0L then
            // printfn "    Current worry level is divisible by %d." note.testDivisibleBy
            note.ifTrue
        else
            // printfn "    Current worry level is not divisible by %d." note.testDivisibleBy
            note.ifFalse

    // printfn "    Item with worry level %d is thrown to monkey %d." item newIndex

    { item = item; newIndex = newIndex }

let addNewItem notes inspection =
    let note = notes |> Map.find inspection.newIndex

    let note =
        { note with startingItems = note.startingItems @ [ inspection.item ] }

    notes |> Map.add inspection.newIndex note

let performMonkeyTurn divideWorry (notes: MonkeyNotes) index : MonkeyNotes =
    // printfn "Monkey %d:" index
    let note = notes |> Map.find index

    let inspections =
        note.startingItems
        |> List.map (inspectItem divideWorry note)

    let note =
        { note with
            startingItems = []
            inspectedItems =
                note.inspectedItems
                + int64 (List.length note.startingItems) }

    let notes = notes |> Map.add index note

    inspections |> List.fold addNewItem notes

let performMonkeyRound divideWorry (notes: MonkeyNotes) : MonkeyNotes =
    notes
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.map fst
    |> Seq.fold (performMonkeyTurn divideWorry) notes

let manageWorryBy manager (notes: MonkeyNotes) : MonkeyNotes =
    notes
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.map (fun note ->
        { note with
            startingItems =
                note.startingItems
                |> List.map (fun item -> item % manager) })
    |> Seq.map (fun note -> note.index, note)
    |> Map.ofSeq

let runGame rounds divideWorry notes =
    let manager =
        notes
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.map (fun note -> note.testDivisibleBy)
        |> Seq.fold (*) 1L

    printfn "Manager: %d" manager

    seq { 1 .. rounds }
    |> Seq.fold
        (fun notes round ->
            let printRound =
                round = rounds
                || round <= 10
                || (round <= 100 && round % 10 = 0)
                || (round <= 1000 && round % 100 = 0)
                || round % 1000 = 0

            let notes =
                notes
                |> performMonkeyRound divideWorry
                |> manageWorryBy manager

            if printRound then
                printfn "After round %d, the monkeys are holding items with these worry levels:" round
                notes |> printNotes |> printfn "%s"

            notes)
        notes

let monkeyBusiness notes =
    let inspectionCounts =
        notes
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.map (fun note -> note.inspectedItems)
        |> Seq.sortDescending
        |> Seq.take 2

    inspectionCounts |> Seq.fold (*) 1L

let part1 lines =
    let notes = takeNotes lines
    let notes = runGame 20 true notes
    let result = monkeyBusiness notes
    sprintf "RESULT = %d" result

let part2 lines =
    let notes = takeNotes lines
    let notes = runGame 10000 false notes
    let result = monkeyBusiness notes
    sprintf "RESULT = %d" result

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllText fileName
|> if part = "1" then part1 else part2
|> printfn "%s"

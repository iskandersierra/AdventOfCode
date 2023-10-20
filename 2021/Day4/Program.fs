open System

type Input =
    { numbers: int []
      boards: BingoBoard [] }

and BingoBoard = int []

let boardSize = 5
let boardLength = boardSize * boardSize

let parseInput (lines: string []) =
    let numbers =
        lines.[0]
        |> String.splitByChar ','
        |> Array.map int

    let mutable index = 1
    let boards = ResizeArray()

    while index < lines.Length && lines.[index] = "" do
        let board = Array.zeroCreate boardLength

        for i = index + 1 to index + 5 do
            let boardLine =
                lines.[i]
                |> String.splitByChar ' '
                |> Array.map int

            Array.blit boardLine 0 board ((i - index - 1) * boardSize) boardSize

        boards.Add(board)
        index <- index + 6

    { numbers = numbers
      boards = boards.ToArray() }

let isBoardRowCompleted markIndex rowIndex marks =
    let mutable isDone = true
    let mutable index = 0

    while isDone && index < boardSize do
        let markIndex = markIndex + rowIndex * boardSize + index
        isDone <- marks |> Array.item markIndex
        index <- index + 1

    isDone

let isBoardColumnCompleted markIndex columnIndex marks =
    let mutable isDone = true
    let mutable index = 0

    while isDone && index < boardSize do
        let markIndex = markIndex + columnIndex + index * boardSize
        isDone <- marks |> Array.item markIndex
        index <- index + 1

    isDone

let isBoardCompleted markIndex marks =
    let mutable isDone = false
    let mutable index = 0

    while not isDone && index < boardSize do
        isDone <-
            isBoardRowCompleted markIndex index marks
            || isBoardColumnCompleted markIndex index marks

        index <- index + 1

    isDone

let markBoard number boardIndex marks input =
    let markIndex = boardIndex * boardLength
    let board = input.boards.[boardIndex]

    match board |> Array.tryFindIndex ((=) number) with
    | Some position ->
        Array.set marks (markIndex + position) true
        isBoardCompleted markIndex marks
    | None -> false

let sumUnmarked boardIndex marks input =
    let markIndex = boardIndex * boardLength
    let board = input.boards.[boardIndex]
    let mutable sum = 0
    for i = 0 to boardLength - 1 do
        if Array.item (markIndex + i) marks |> not then
            sum <- sum + board.[i]
    sum

let step1 input =
    let marks =
        Array.zeroCreate (input.boards.Length * boardLength)

    let mutable score = 0
    let mutable index = 0
    let mutable completed = false

    while not completed && index < input.numbers.Length do
        let number = input.numbers.[index]
        let mutable boardIndex = 0

        while not completed && boardIndex < input.boards.Length do
            completed <- markBoard number boardIndex marks input
            if completed then
                let unmarked = sumUnmarked boardIndex marks input
                score <- number * unmarked
            else
                boardIndex <- boardIndex + 1

        index <- index + 1

    score

let step2 input =
    let marks =
        Array.zeroCreate (input.boards.Length * boardLength)
    let completedBoards =
        Array.zeroCreate input.boards.Length
    let mutable completedBoardsCount = 0

    let mutable score = 0
    let mutable index = 0
    let mutable completed = false

    while not completed && index < input.numbers.Length do
        let number = input.numbers.[index]
        let mutable boardIndex = 0

        while not completed && boardIndex < input.boards.Length do
            if not completedBoards.[boardIndex] then
                let boardCompleted = markBoard number boardIndex marks input
                if boardCompleted then
                    completedBoards.[boardIndex] <- true
                    completedBoardsCount <- completedBoardsCount + 1
                    if completedBoardsCount = input.boards.Length then
                        completed <- true
                        let unmarked = sumUnmarked boardIndex marks input
                        score <- number * unmarked
            boardIndex <- boardIndex + 1

        index <- index + 1

    score

let main () =
    let input = Input.parseInputLines () |> parseInput

    let n =
        input.numbers.Length * input.boards.Length

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1: %d" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2: %d" result2

main ()

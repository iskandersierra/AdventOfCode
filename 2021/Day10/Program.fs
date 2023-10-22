open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = string []

type Output1 =
    { result: uint64
      lines: LineScore [] }
    override this.ToString() =
        let sb = new StringBuilder()

        sb
            .Append($"Lines [{this.lines.Length}]:")
            .AppendLine()
        |> ignore

        this.lines
        |> Array.iter (fun line -> sb.Append(line.ToString()) |> ignore)

        sb
            .Append("Result: ")
            .Append(this.result)
            .AppendLine()
        |> ignore

        sb.ToString()

and LineScore =
    { line: string
      points: uint64
      result: Result<{| completion: string |}, {| found: char
                                                  expected: char voption
                                                  at: int |}> }
    override this.ToString() =
        let sb = new StringBuilder()

        sb
            .Append($"% 7i{this.points}")
            .Append(" - ")
            .Append(this.line)
        |> ignore

        match this.result with
        | Ok v ->
            sb
                .Append(" - Complete with ")
                .Append(v.completion)
                .AppendLine()
        | Error result ->
            sb
                .Append(" - Mismatch: ")
                .Append(" expected ")
                .Append(
                    match result.expected with
                    | ValueSome ch -> ch.ToString()
                    | ValueNone -> "None"
                )
                .Append(" but found ")
                .Append(result.found)
                .Append(" at ")
                .Append(result.at)
                .AppendLine()
        |> ignore

        sb.ToString()

let wrongCharPoints =
    [ ')', 3
      ']', 57
      '}', 1197
      '>', 25137 ]
    |> Map.ofList

let completionCharPoints =
    [ ')', 1
      ']', 2
      '}', 3
      '>', 4 ]
    |> Map.ofList

let charPairs =
    [ '(', ')'
      '[', ']'
      '{', '}'
      '<', '>' ]
    |> Map.ofList

let parseInput (text: string []) : Input = text

let computeLinePoints (line: string) =
    let rec loop index closeStack =
        if index >= line.Length then
            Ok closeStack
        else
            let currentCh = line.[index]

            match charPairs |> Map.tryFind currentCh with
            | Some closedCh ->
                // Then currentCh is an open char, lets add its closed char to the stack
                loop (index + 1) (closedCh :: closeStack)
            | None ->
                // Then currentCh is a closed char, lets check if it matches the last open char in the stack
                match closeStack with
                | nextCh :: nextStack when nextCh = currentCh ->
                    // Then we have a match, lets remove the last open char from the stack
                    loop (index + 1) nextStack
                | nextCh :: _ ->
                    // Then we have a mismatch, lets return the points for this char and finish
                    Error
                        {| found = currentCh
                           expected = ValueSome nextCh
                           at = index |}
                | [] ->
                    // Then we have a mismatch, lets return the points for this char and finish
                    Error
                        {| found = currentCh
                           expected = ValueNone
                           at = index |}

    loop 0 []


let solve (input: Input) =
    let lines = ResizeArray()

    for i = 0 to input.Length - 1 do
        let line = input.[i]

        match computeLinePoints line with
        | Ok result ->
            let points =
                result
                |> List.fold (fun acc ch -> 5UL * acc + uint64 completionCharPoints.[ch]) 0UL

            lines.Add(
                { line = line
                  points = points
                  result = Ok {| completion = System.String (result |> List.toArray) |} }
            )

        | Error result ->
            let points = uint64 wrongCharPoints.[result.found]

            lines.Add(
                { line = line
                  points = points
                  result = Error result }
            )

    let lines = lines.ToArray()

    let result =
        lines |> Array.sumBy (fun line -> line.points)

    { Output1.result = result
      lines = lines }


let step1 (input: Input) =
    let solution = solve input

    // Take only the errored lines and add their points
    let totalPoints =
        solution.lines
        |> Array.filter (fun line -> match line.result with | Error _ -> true | _ -> false)
        |> Array.sumBy (fun line -> line.points)

    { solution with result = totalPoints }

let step2 (input: Input) =
    let solution = solve input

    // Take only the correct lines, sort by points and take the middle one
    let totalPoints =
        solution.lines
        |> Array.filter (fun line -> match line.result with | Ok _ -> true | _ -> false)
        |> Array.sortBy (fun line -> line.points)
        |> (fun lines -> lines.[lines.Length / 2].points)

    { solution with result = totalPoints }


let main () =
    let input = Input.parseInputLines () |> parseInput

    let n = Array.length input

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1:\n%O" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2:\n%O" result2

main ()

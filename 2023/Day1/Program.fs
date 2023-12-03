open System
open System.Numerics
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = string []

type OptimInput = string []

let parseInput (text: string []) = text

let optimizeInput (input: Input) = input

let digits = [| '0' .. '9' |]

let computeLineValue (line: string) =
    let firstPos = line.IndexOfAny digits
    let lastPos = line.LastIndexOfAny digits
    let firstDigit = int (line.[firstPos] - '0')
    let lastDigit = int (line.[lastPos] - '0')
    firstDigit * 10 + lastDigit

let digitRegex =
    Regex(@"\d|one|two|three|four|five|six|seven|eight|nine", RegexOptions.Compiled)

let digitValues =
    [ "one", 1
      "two", 2
      "three", 3
      "four", 4
      "five", 5
      "six", 6
      "seven", 7
      "eight", 8
      "nine", 9 ]
    |> Map.ofList

let getDigitValue (digit: string) =
    digitValues |> Map.tryFind digit |> Option.defaultWith (fun () -> Int32.Parse(digit))

let computeLineValue2 (line: string) =
    let matches = digitRegex.Matches(line)
    let firstMatch = matches.[0].Value
    let lastMatch = matches.[matches.Count - 1].Value
    let firstDigit = getDigitValue firstMatch
    let lastDigit = getDigitValue lastMatch
    firstDigit * 10 + lastDigit

let step1 (input: OptimInput) =
    let mutable result = 0

    for line in input do
        let value = computeLineValue line
        result <- result + value

    result

let step2 (input: OptimInput) =
    let mutable result = 0

    for line in input do
        let value = computeLineValue2 line
        result <- result + value

    result

let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let input = optimizeInput input
    printfn "OptimInput:\n%O" input

    let n = input.Length
    let time = Time.measureN

    (fun () -> step1 input)
    |> time n
    |> printfn "***** STEP1:\n%O"

    (fun () -> step2 input)
    |> time n
    |> printfn "***** STEP2:\n%A"

main ()

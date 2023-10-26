open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input = string []

and OptimInput = { numbers: Number [] }

and Number =
    | RegularNumber of value: int
    | PairNumber of left: Number * right: Number


let parseInput (text: string[]) = text


type Parser = { text: string; index: int }

type ParseResult<'a> = ('a * Parser) option

type Parse<'a> = Parser -> ParseResult<'a>

let parseCharFn (predicate: char -> bool) (parser: Parser) : ParseResult<char> =
    if parser.index >= parser.text.Length then
        None
    else
        let index = parser.index
        let ch = parser.text.[index]

        if predicate ch then
            Some(ch, { parser with index = index + 1 })
        else
            None

let parseChar (ch: char) = parseCharFn ((=) ch)

let parseDigits (parser: Parser) : ParseResult<string> =
    let isDigit (ch: char) = ch >= '0' && ch <= '9'

    let rec loop (parser: Parser) (digits: StringBuilder) =
        match parseCharFn isDigit parser with
        | None ->
            if digits.Length = 0 then
                None
            else
                Some (digits.ToString(), parser)
        | Some (ch, parser) ->
            digits.Append ch |> ignore
            loop parser digits

    loop parser (StringBuilder())

let parseRegularNumber (parser: Parser) : ParseResult<Number> =
    match parseDigits parser with
    | None -> None
    | Some (digits, parser) ->
        let value = int digits
        Some (RegularNumber value, parser)

let rec parsePairNumber (parser: Parser) : ParseResult<Number> =
    optional {
        let! _, parser = parseChar '[' parser
        let! left, parser = parseNumber parser
        let! _, parser = parseChar ',' parser
        let! right, parser = parseNumber parser
        let! _, parser = parseChar ']' parser
        return PairNumber (left, right), parser
    }

and parseNumber (parser: Parser) : ParseResult<Number> =
    match parseRegularNumber parser with
    | Some (number, parser) -> Some (number, parser)
    | None ->
        match parsePairNumber parser with
        | Some (number, parser) -> Some (number, parser)
        | None -> None

let optimizeInput (input: Input) =
    let numbers =
        input
        |> Array.map (fun line ->
            let parser = { text = line; index = 0 }
            match parseNumber parser with
            | Some (number, _) -> number
            | None -> failwithf "Failed to parse line: %s" line
        )
    { numbers = numbers }

let printNumber (number: Number) =
    let rec loop number (sb: StringBuilder) =
        match number with
        | RegularNumber value ->
            sb.Append value |> ignore
        | PairNumber (left, right) ->
            sb.Append '[' |> ignore
            loop left sb
            sb.Append ',' |> ignore
            loop right sb
            sb.Append ']' |> ignore
    let sb = StringBuilder()
    loop number sb
    sb.ToString()

let rec magnitude (number: Number) =
    match number with
    | RegularNumber value -> value
    | PairNumber (left, right) -> 3 * (magnitude left) + 2 * (magnitude right)

let addToLeftmostValue number toAdd =
    let rec loop number =
        match number with
        | RegularNumber value -> RegularNumber (value + toAdd)
        | PairNumber (left, right) -> PairNumber (loop left, right)
    if toAdd = 0 then
        number
    else
        loop number

let addToRightmostValue number toAdd =
    let rec loop number =
        match number with
        | RegularNumber value -> RegularNumber (value + toAdd)
        | PairNumber (left, right) -> PairNumber (left, loop right)
    if toAdd = 0 then
        number
    else
        loop number

type ExplodeNumberResult =
    | Unexploded of result: Number
    | Exploding of result: Number * left: int * right: int

let explodeNumber number =
    let rec loop depth number =
        match number with
        | RegularNumber _ ->
            Unexploded number
        | PairNumber (RegularNumber left, RegularNumber right) when depth >= 4 ->
            Exploding (RegularNumber 0, left, right)
        | PairNumber (left, right) ->
            match loop (depth + 1) left with
            | Exploding(exploded, leftValue, rightValue) ->
                let rightNumber = addToLeftmostValue right rightValue
                Exploding (PairNumber (exploded, rightNumber), leftValue, 0)
            | Unexploded leftNumber ->
                match loop (depth + 1) right with
                | Exploding(exploded, leftValue, rightValue) ->
                    let leftNumber = addToRightmostValue left leftValue
                    Exploding (PairNumber (leftNumber, exploded), 0, rightValue)
                | Unexploded rightNumber ->
                    Unexploded (PairNumber (leftNumber, rightNumber))
    loop 0 number

type SplitNumberResult =
    | Unsplit of result: Number
    | Split of result: Number

let rec splitNumber number =
    match number with
    | RegularNumber value when value >= 10 ->
        let left = value / 2
        let right = value - left
        Split (PairNumber (RegularNumber left, RegularNumber right))
    | RegularNumber value ->
        Unsplit number
    | PairNumber (leftNumber, rightNumber) ->
        match splitNumber leftNumber with
        | Split leftNumber ->
            Split (PairNumber (leftNumber, rightNumber))
        | Unsplit leftNumber ->
            match splitNumber rightNumber with
            | Split rightNumber ->
                Split (PairNumber (leftNumber, rightNumber))
            | Unsplit rightNumber ->
                Unsplit (PairNumber (leftNumber, rightNumber))

let reduceNumber (number: Number) =
    let rec loop current =
        match explodeNumber current with
        | Exploding (exploded, _, _) ->
            // printfn "Exploded %s to %s" (printNumber current) (printNumber exploded)
            loop exploded
        | Unexploded result ->
            match splitNumber result with
            | Split result ->
                // printfn "Split %s to %s" (printNumber current) (printNumber result)
                loop result
            | Unsplit result ->
                result
    loop number


let step1 (input: OptimInput) =
    let rec loop accumulator index =
        if index >= input.numbers.Length then
            accumulator
        else
            let right = input.numbers.[index]
            let sum = PairNumber (accumulator, right)
            let reduced = reduceNumber sum
            // printfn "Exploded %s to %s" (printNumber sum) (printNumber reduced)
            loop reduced (index + 1)
    let result = loop input.numbers.[0] 1
    // printfn "Result: %s" (printNumber result)
    magnitude result

let step2 (input: OptimInput) =
    let mutable maxMagnitude = 0
    for i = 0 to input.numbers.Length - 1 do
        let left = input.numbers.[i]
        for j = 0 to input.numbers.Length - 1 do
            let right = input.numbers.[j]
            if i <> j then
                let sum = PairNumber (left, right)
                let reduced = reduceNumber sum
                let magnitude = magnitude reduced
                if magnitude > maxMagnitude then
                    maxMagnitude <- magnitude
                    // printfn "New max magnitude: %d" magnitude
    maxMagnitude


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let input = optimizeInput input
    // printfn "OptimInput:\n%A" input

    let n = 1
    let time = Time.measureColdN

    (fun () -> step1 input)
    |> time n
    |> printfn "***** STEP1:\n%O"

    (fun () -> step2 input)
    |> time n
    |> printfn "***** STEP2:\n%O"


main ()

open System
open System.Text.RegularExpressions

type Input = Note []

and [<Struct>] Note =
    { inputSignals: string []
      outputSignals: string [] }

(*
| Digit | Segments | Count | Cannot be |
|-------|----------|-------|-----------|
| 0     | abcefg   | 6     | d         |
| 1     | cf       | 2  u  | abdeg     |
| 2     | acdeg    | 5     | bf        |
| 3     | acdfg    | 5     | be        |
| 4     | bcdf     | 4  u  | aeg       |
| 5     | abdfg    | 5     | ce        |
| 6     | abdefg   | 6     | c         |
| 7     | acf      | 3  u  | bdeg      |
| 8     | abcdefg  | 7  u  | -         |
| 9     | abcdfg   | 6     | e         |

| Count | Digits | Cannot be |
| 2     | 1      | abdeg     |
| 3     | 7      | bdeg      |
| 4     | 4      | aeg       |
| 5     | 235    | bcef      |
| 6     | 069    | -         |
| 7     | 8      | -         |
*)

type DigitData =
    { digit: int // 0..9
      segments: int [] }

let digitData =
    [| { digit = 0
         segments = [| 0; 1; 2; 4; 5; 6 |] }
       { digit = 1; segments = [| 2; 5 |] }
       { digit = 2
         segments = [| 0; 2; 3; 4; 6 |] }
       { digit = 3
         segments = [| 0; 2; 3; 5; 6 |] }
       { digit = 4
         segments = [| 1; 2; 3; 5 |] }
       { digit = 5
         segments = [| 0; 1; 3; 5; 6 |] }
       { digit = 6
         segments = [| 0; 1; 3; 4; 5; 6 |] }
       { digit = 7; segments = [| 0; 2; 5 |] }
       { digit = 8
         segments = [| 0; 1; 2; 3; 4; 5; 6 |] }
       { digit = 9
         segments = [| 0; 1; 2; 3; 5; 6 |] } |]

let noteRegex =
    Regex(@"^((?<input>[a-g]+)\s)+\|(\s(?<output>[a-g]+))+$", RegexOptions.Compiled)

let parseNote (line: string) =
    let m = noteRegex.Match(line)

    if m.Success then
        let inputSignals =
            m.Groups.["input"].Captures
            |> Seq.cast<Capture>
            |> Seq.map (fun c -> c.Value)
            |> Seq.toArray

        let outputSignals =
            m.Groups.["output"].Captures
            |> Seq.cast<Capture>
            |> Seq.map (fun c -> c.Value)
            |> Seq.toArray

        { inputSignals = inputSignals
          outputSignals = outputSignals }
    else
        failwithf "Invalid note: %s" line


let parseInput (text: string []) = text |> Array.map parseNote

let countEasyDigits (output: string []) =
    let mutable count = 0

    for i = 0 to output.Length - 1 do
        match output.[i].Length with
        | 2
        | 3
        | 4
        | 7 -> count <- count + 1
        | _ -> ()

    count

let decodeSlow combination (signal: string) =
    signal
    |> Seq.map (fun ch -> int ch - int 'a')
    |> Seq.map (fun ch -> Array.get combination ch)
    |> Seq.sort
    |> Seq.toArray

let decode (combination: int []) (signal: string) =
    let arr =
        Array.init signal.Length (fun i -> combination.[int signal.[i] - int 'a'])

    Array.Sort(arr)
    arr

let isValidDigit (combination: int []) (signal: string) =
    let decoded = decode combination signal

    digitData
    |> Array.exists (fun d -> d.segments = decoded)

let isValidCombinationSlow (note: Note) (combination: int []) =
    Seq.concat [ note.inputSignals
                 note.outputSignals ]
    |> Seq.forall (fun signal -> isValidDigit combination signal)

let isValidCombination (note: Note) (combination: int []) =
    (note.inputSignals
     |> Array.forall (isValidDigit combination))
    && (note.outputSignals
        |> Array.forall (isValidDigit combination))

let evaluateOutput signals combination =
    signals
    |> Seq.map (fun signal ->
        let decoded = decode combination signal

        digitData
        |> Array.find (fun d -> d.segments = decoded))
    |> Seq.fold (fun acc digit -> acc * 10 + digit.digit) 0

let computeOutput (note: Note) =
    Array.iterateCombinations
        (fun c ->
            if isValidCombination note c then
                ValueSome c
            else
                ValueNone)
        7
        7
    |> function
        | ValueSome combination -> evaluateOutput note.outputSignals combination
        | ValueNone -> failwithf "No valid combination found for note: %A" note

let step1 (input: Input) =
    let mutable count = 0

    for index = 0 to input.Length - 1 do
        let easyCount =
            countEasyDigits input.[index].outputSignals

        count <- count + easyCount

    count

let step2 (input: Input) =
    let mutable sumOutput = 0

    for index = 0 to input.Length - 1 do
        let noteOutput = computeOutput input.[index]
        sumOutput <- sumOutput + noteOutput

    sumOutput

let main () =
    let input = Input.parseInputLines () |> parseInput

    let n = input.Length

    let result1 = Time.measureN n (fun () -> step1 input)
    printfn "***** STEP1: %d" result1

    let result2 = Time.measureN n (fun () -> step2 input)
    printfn "***** STEP2: %d" result2

main ()

open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input =
    { playerStart1: int
      playerStart2: int }

and OptimInput = Input

let inputLineRegex =
    Regex(@"^Player (?<idx>1|2) starting position: (?<pos>\d+)$")

let parseInput (text: string []) =
    let getStart (line: string) =
        match inputLineRegex.Match line with
        | m when m.Success ->
            let pos = int m.Groups.["pos"].Value
            pos
        | _ -> failwithf "Invalid input line: %s" line

    let playerStart1 = getStart text.[0]
    let playerStart2 = getStart text.[1]

    { playerStart1 = playerStart1
      playerStart2 = playerStart2 }

let optimizeInput (input: Input) = input

let step1 (input: OptimInput) =
    let diceSize = 100
    let boardSize = 10
    let maxScore = 1000

    let rec loop player pos otherPos score otherScore diceIndex diceRolls =
        let dice1 = diceIndex + 1
        let dice2 = (diceIndex + 1) % diceSize + 1
        let dice3 = (diceIndex + 2) % diceSize + 1
        let diceRolls' = diceRolls + 3
        let diceIndex' = (diceIndex + 3) % diceSize

        let pos' =
            (pos + dice1 + dice2 + dice3) % boardSize

        let score' = score + pos' + 1
        // printfn "- Player %d rolls %d+%d+%d and moves to space %d for a total score of %d" player dice1 dice2 dice3 (pos' + 1) score'
        if score' < maxScore then
            loop (3 - player) otherPos pos' otherScore score' diceIndex' diceRolls'
        else
            otherScore * diceRolls'

    loop 1 (input.playerStart1 - 1) (input.playerStart2 - 1) 0 0 0 0

[<Struct>]
type SearchKey =
    { player: int
      pos: int
      otherPos: int
      score: int
      otherScore: int }

[<Struct>]
type Wins =
    { wins1: uint64
      wins2: uint64 }
    static member (+)(a: Wins, b: Wins) =
        { wins1 = a.wins1 + b.wins1
          wins2 = a.wins2 + b.wins2 }

let step2 (input: OptimInput) =
    let diceSize = 3
    let boardSize = 10
    let maxScore = 21
    let oneWins = { wins1 = 1UL; wins2 = 0UL }
    let twoWins = { wins1 = 0UL; wins2 = 1UL }
    let memo = Dictionary<SearchKey, Wins>()

    let rec countWins key =
        match memo.TryGetValue key with
        | true, wins -> wins
        | false, _ ->
            let mutable wins = { wins1 = 0UL; wins2 = 0UL }

            for die1 = 1 to diceSize do
                for die2 = 1 to diceSize do
                    for die3 = 1 to diceSize do
                        let pos' =
                            (key.pos + die1 + die2 + die3) % boardSize

                        let score' = key.score + pos' + 1

                        if score' >= maxScore then
                            match key.player with
                            | 1 -> wins <- wins + oneWins
                            | _ -> wins <- wins + twoWins
                        else
                            let key' =
                                { player = 3 - key.player
                                  pos = key.otherPos
                                  otherPos = pos'
                                  score = key.otherScore
                                  otherScore = score' }

                            let wins' = countWins key'

                            wins <- wins + wins'

            memo.Add(key, wins)
            wins

    countWins { player = 1
                pos = input.playerStart1 - 1
                otherPos = input.playerStart2 - 1
                score = 0
                otherScore = 0 }


let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let input = optimizeInput input
    // printfn "OptimInput:\n%O" input

    let n = 1
    let time = Time.measureN

    (fun () -> step1 input)
    |> time n
    |> printfn "***** STEP1:\n%O"

    (fun () -> step2 input)
    |> time n
    |> printfn "***** STEP2:\n%A"


main ()

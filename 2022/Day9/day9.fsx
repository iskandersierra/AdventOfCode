open System
open System.IO
open System.Text
open System.Text.RegularExpressions

#nowarn "0025"

type HeadMove =
    | Right
    | Left
    | Up
    | Down

let (|HeadMultiMove|_|) line =
    let m =
        Regex.Match(line, @"^(?<direction>[LRUD])\s(?<distance>\d+)$")

    if m.Success then
        let direction = m.Groups.["direction"].Value
        let distance = int m.Groups.["distance"].Value

        match direction with
        | "R" -> Some(Right, distance)
        | "L" -> Some(Left, distance)
        | "U" -> Some(Up, distance)
        | "D" -> Some(Down, distance)
        | _ -> None
    else
        None

let readHeadMoves lines =
    seq {
        for line in lines do
            match line with
            | HeadMultiMove (move, distance) ->
                for _ in 1 .. distance do
                    yield move
            | _ -> failwithf "Invalid head move: %s" line
    }

let minMax values =
    values
    |> Seq.fold
        (fun acc value ->
            match acc with
            | None -> Some(value, value)
            | Some (minimum, maximum) -> Some(min value minimum, max value maximum))
        None

type SimulationState =
    { head: int * int
      tail: (int * int) list
      visited: Set<int * int> }
    member this.GetSymbol(x, y) =
        if (x, y) = this.head then
            "H"
        else
            match List.tryFindIndex ((=) (x, y)) this.tail with
            | Some index -> string (index + 1)
            | None ->
                if (x, y) = (0, 0) then
                    "s"
                elif this.visited.Contains(x, y) then
                    "#"
                else
                    "."

    override this.ToString() =
        let allPoints =
            [ yield! this.visited
              yield this.head
              yield! this.tail ]

        let minX, maxX =
            allPoints
            |> Seq.map fst
            |> minMax
            |> Option.defaultValue (0, 0)

        let minY, maxY =
            allPoints
            |> Seq.map snd
            |> minMax
            |> Option.defaultValue (0, 0)

        let sb = StringBuilder()

        for y in maxY .. -1 .. minY do
            for x in minX .. maxX do
                sb.Append(this.GetSymbol(x, y)) |> ignore

            sb.AppendLine() |> ignore

        sb.ToString()

let startState size =
    { head = 0, 0
      tail = [ 1 .. size ] |> List.map (fun _ -> 0, 0)
      visited = Set.singleton (0, 0) }

let getClose1D distance head tail =
    if abs (head - tail) >= distance then
        tail + sign (head - tail)
    else
        tail

let updateTail head tail =
    match tail, head with
    | (xt, yt), (xh, yh) ->
        if abs (xh - xt) <= 1 && abs (yh - yt) <= 1 then
            (xt, yt)
        elif xt = xh then
            xt, getClose1D 2 yh yt
        elif yt = yh then
            getClose1D 2 xh xt, yt
        else
            getClose1D 1 xh xt, getClose1D 1 yh yt

let updateState state move =
    let head' =
        match move, state.head with
        | Right, (x, y) -> x + 1, y
        | Left, (x, y) -> x - 1, y
        | Up, (x, y) -> x, y + 1
        | Down, (x, y) -> x, y - 1

    let tail' =
        let rec loop head tail =
            match tail with
            | [] -> []
            | item :: rest ->
                let item' = updateTail head item
                item' :: loop item' rest
        loop head' state.tail

    let visited' = state.visited.Add (List.last tail')

    { head = head'
      tail = tail'
      visited = visited' }

let part1 lines =
    lines
    |> readHeadMoves
    |> Seq.scan updateState (startState 1)
    |> Seq.last
    |> fun state -> state.visited.Count
    // |> Seq.iter (fun state -> printfn "%O%d\n" state state.visited.Count)

let part2 lines =
    lines
    |> readHeadMoves
    |> Seq.scan updateState (startState 9)
    |> Seq.last
    |> fun state -> printfn "%O" state; state
    |> fun state -> state.visited.Count
    // |> Seq.iter (fun state -> printfn "%O%d\n" state state.visited.Count)

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllLines fileName
|> if part = "1" then part1 else part2
|> printfn "%A"

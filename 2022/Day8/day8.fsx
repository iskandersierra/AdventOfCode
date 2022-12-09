open System
open System.IO

#nowarn "0025"

let readGrid lines =
    lines
    |> Seq.filter (fun line -> line <> "")
    |> Seq.map (fun line -> line.ToCharArray() |> Array.map (fun c -> int c - int '0'))
    |> Seq.toArray
    |> fun data ->
        let len1 = data.Length
        let len2 = data.[0].Length
        let grid = Array2D.zeroCreate len1 len2
        for y = 0 to len2 - 1 do
            for x = 0 to len1 - 1 do
                grid.[x, y] <- data.[x].[y]
        grid

let isHidden x y (grid: int[,]) =
    let height = grid.[x, y]
    let len1 = grid.GetLength(0)
    let len2 = grid.GetLength(1)
    if x <= 0 || x >= len1 - 1 || y <= 0 || y >= len2 - 1 then false
    else
        let hiddenByHeight = List.exists (fun h -> h >= height)
        let hiddenNorth =
            [ for i in y - 1 .. -1 .. 0 -> grid.[x, i] ] |> hiddenByHeight
        let hiddenSouth =
            [ for i in y + 1 .. len2 - 1 do grid.[x, i] ] |> hiddenByHeight
        let hiddenWest =
            [ for i in x - 1 .. -1 .. 0 -> grid.[i, y] ] |> hiddenByHeight
        let hiddenEast =
            [ for i in x + 1 .. len1 - 1 do grid.[i, y] ] |> hiddenByHeight
        hiddenNorth && hiddenSouth && hiddenWest && hiddenEast

let countVisible (grid: int[,]) =
    let len1 = grid.GetLength(0)
    let len2 = grid.GetLength(1)
    [ for x = 0 to len1 - 1 do
        for y = 0 to len2 - 1 do
            if isHidden x y grid then 0 else 1 ]
    |> List.sum

let scenicScore x y (grid: int[,]) =
    let height = grid.[x, y]
    let len1 = grid.GetLength(0)
    let len2 = grid.GetLength(1)
    if x <= 0 || x >= len1 - 1 || y <= 0 || y >= len2 - 1 then 0
    else
        let rec scoreByHeight score = function
            | [] -> score
            | h :: _ when h >= height -> score + 1
            | _ :: hs -> scoreByHeight (score + 1) hs

        let scoreNorth = [ for i in y - 1 .. -1 .. 0 -> grid.[x, i] ] |> scoreByHeight 0
        let scoreSouth = [ for i in y + 1 .. len2 - 1 do grid.[x, i] ] |> scoreByHeight 0
        let scoreWest = [ for i in x - 1 .. -1 .. 0 -> grid.[i, y] ] |> scoreByHeight 0
        let scoreEast = [ for i in x + 1 .. len1 - 1 do grid.[i, y] ] |> scoreByHeight 0
        scoreNorth * scoreSouth * scoreWest * scoreEast

let findMaxScenicScore (grid: int[,]) =
    let len1 = grid.GetLength(0)
    let len2 = grid.GetLength(1)
    [ for x = 0 to len1 - 1 do
        for y = 0 to len2 - 1 do
            scenicScore x y grid ]
    |> List.max

let part1 lines =
    lines
    |> readGrid
    |> countVisible

let part2 lines =
    lines
    |> readGrid
    |> findMaxScenicScore

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllLines fileName
    |> if part = "1" then part1 else part2
    |> printfn "%A"

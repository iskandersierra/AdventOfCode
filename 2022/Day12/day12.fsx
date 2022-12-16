#load "../../modules/AStarSearch.fsx"

open System
open System.IO
open System.Collections.Generic
open System.Text
open System.Text.RegularExpressions
open AStarSearch

#nowarn "0025"

type Terrain = Terrain of start: (int * int) * finish: (int * int) * elevation: int [,]

let readTerrain (lines: string []) =
    let height = lines.Length
    let width = lines.[0].Length
    let terrain = Array2D.zeroCreate height width
    let mutable start = (0, 0)
    let mutable finish = (0, 0)

    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            let ch = lines.[y].[x]

            if ch = 'S' then
                terrain.[y, x] <- 0
                start <- (x, y)
            elif ch = 'E' then
                terrain.[y, x] <- int 'z' - int 'a'
                finish <- (x, y)
            else
                terrain.[y, x] <- int ch - int 'a'

    Terrain(start, finish, terrain)

let findShortestPath part (Terrain (start, finish, terrain)) =
    let height = terrain.GetLength(0)
    let width = terrain.GetLength(1)

    let startCells =
        if part = 1 then
            [ { cell = start
                value = terrain.[snd start, fst start] } ]
        else
            [ for y = 0 to height - 1 do
                  for x = 0 to width - 1 do
                      if terrain.[y, x] = 0 then
                          yield { cell = (x, y); value = 0 } ]

    let terrainFn fromCell =
        let (fromX, fromY) = fromCell.cell

        [ (fromX - 1, fromY)
          (fromX + 1, fromY)
          (fromX, fromY - 1)
          (fromX, fromY + 1) ]
        |> List.filter (fun (x, y) -> x >= 0 && x < width && y >= 0 && y < height)
        |> List.filter (fun (x, y) ->
            terrain.[y, x] <= terrain.[fromY, fromX]
            || terrain.[y, x] - terrain.[fromY, fromX] <= 1)
        |> List.map (fun cell ->
            { cell = cell
              value = fromCell.value + 1 })

    let path =
        astar
            { startCells = startCells
              isEndCell = fun cell -> finish = cell
              cellComparer = EqualityComparer<_>.Default
              valueComparer = Comparer<_>.Default
              terrainFn = terrainFn
              debug = None }

    path

let printTerrain (Terrain (start, finish, terrain)) =
    let height = terrain.GetLength(0)
    let width = terrain.GetLength(1)

    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            let ch =
                if (x, y) = start then 'S'
                elif (x, y) = finish then 'E'
                else char (int 'a' + terrain.[y, x])

            printf "%c" ch

        printfn ""

let printPath (Terrain (start, finish, terrain)) path =
    let height = terrain.GetLength(0)
    let width = terrain.GetLength(1)

    for y = 0 to height - 1 do
        for x = 0 to width - 1 do
            let ch =
                if (x, y) = start then
                    'S'
                elif (x, y) = finish then
                    'E'
                elif path
                     |> List.exists (fun cell -> cell.cell = (x, y)) then
                    'X'
                else
                    char (int 'a' + terrain.[y, x])

            printf "%c" ch

        printfn ""

let printPathList (Terrain (start, finish, terrain)) path =
    path
    |> List.map (fun cell -> cell.cell)
    |> List.iter (fun (x, y) ->
        let atCh =
            if (x, y) = start then 'S'
            elif (x, y) = finish then 'E'
            else char (int 'a' + terrain.[y, x])

        printfn "(%d, %d) at '%c' with value %d" x y atCh terrain.[y, x])

let showPath terrain path =
    match path with
    | None ->
        printfn "*** No path found ***"
        -1
    | Some path ->
        printPath terrain path
        printPathList terrain path
        path.Length - 1

let part1 lines =
    let terrain = readTerrain lines
    printTerrain terrain
    let path = findShortestPath 1 terrain
    printfn ""
    let pathLength = showPath terrain path
    sprintf "%d" pathLength

let part2 lines =
    let terrain = readTerrain lines
    printTerrain terrain
    let path = findShortestPath 2 terrain
    printfn ""
    let pathLength = showPath terrain path
    sprintf "%d" pathLength

let [| fileName; part |] = Environment.GetCommandLineArgs().[2..3]

File.ReadAllLines fileName
|> if part = "1" then part1 else part2
|> printfn "%s"

let addSorted maxSize value list =
    value :: list
    |> List.sortDescending
    |> function
        | l when List.length l > maxSize -> List.take maxSize l
        | l -> l

System.Environment.GetCommandLineArgs().[2]
|> System.IO.File.ReadAllLines
|> Seq.map System.Int32.TryParse
|> Seq.map (function
    | true, x -> Some x
    | _ -> None)
|> Seq.toList
|> Seq.fold
    (fun (maxList, current) value ->
        match value with
        | None -> (addSorted 3 current maxList, 0)
        | Some x -> (maxList, current + x))
    ([], 0)
|> fst
|> List.sum
|> printfn "%A"

System.Environment.GetCommandLineArgs().[2]
|> System.IO.File.ReadAllLines
|> Seq.map System.Int32.TryParse
|> Seq.map (function true, x -> Some x | _ -> None)
|> Seq.fold
    (fun (maximum, current) -> function
        | None -> (max maximum current, 0)
        | Some value -> (maximum, current + value))
    (0, 0)
|> fst
|> printfn "%A"

let computeIncreases windowSize numbers =
    numbers
    |> Array.windows windowSize
    |> Seq.toArray
    |> Array.windows 2
    // |> Seq.tee (fun w -> printfn "%A" w)
    |> Seq.map (fun w -> Seq.sum w.[0], Seq.sum w.[1])
    // |> Seq.tee (fun w -> printfn "%A" w)
    |> Seq.filter (fun (a, b) -> a < b)
    // |> Seq.tee (fun w -> printfn "Increased")
    |> Seq.length

let inline sumWindow init finish numbers =
    let mutable sum = 0
    for i = init to finish do
        sum <- sum + Array.item i numbers
    sum

let computeIncreasesProc windowSize numbers =
    let mutable increaseCount = 0
    for i = 0 to Array.length numbers - windowSize - 1 do
        let window1Start = i
        let window1End = i + windowSize - 1
        let window2Start = i + 1
        let window2End = i + windowSize
        let sum1 = sumWindow window1Start window1End numbers
        let sum2 = sumWindow window2Start window2End numbers
        if sum1 < sum2 then
            increaseCount <- increaseCount + 1
    increaseCount

let main() =
    let input =
        Input.parseInputLines()
        |> Array.map int

    let n = Array.length input

    let result1 = Time.measureN n (fun () -> computeIncreases 1 input)
    printfn "Increases: %d" result1

    let result1 = Time.measureN n (fun () -> computeIncreasesProc 1 input)
    printfn "Increases: %d" result1

    let result2 = Time.measureN n (fun () -> computeIncreases 3 input)
    printfn "Window increases: %d" result2

    let result2 = Time.measureN n (fun () -> computeIncreasesProc 3 input)
    printfn "Window increases: %d" result2

main()

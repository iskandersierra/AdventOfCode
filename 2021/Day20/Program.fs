open System
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic

type Input =
    { algorithm: bool []
      background: bool
      width: int
      height: int
      image: bool [] }
    member this.CountPixels(value: bool) =
        this.image
        |> Array.filter (fun v -> v = value)
        |> Array.length
    member inline this.GetIndex x y = y * this.width + x
    member inline this.GetPixel x y =
        if x < 0 || x >= this.width || y < 0 || y >= this.height then
            this.background
        else
            this.image.[this.GetIndex x y]

    member inline this.GetPosition index = (index % this.width, index / this.width)
    override this.ToString() =
        let sb = StringBuilder()
        for y = 0 to this.height - 1 do
            for x = 0 to this.width - 1 do
                let char = if this.image.[y * this.width + x] then '#' else '.'
                sb.Append(char) |> ignore
            sb.AppendLine() |> ignore
        sb.AppendLine($"Count lit pixels: {this.CountPixels(true)}") |> ignore
        sb.AppendLine($"Count unlit pixels: {this.CountPixels(false)}") |> ignore
        sb.ToString()

and OptimInput = Input

let parseInput (text: string []) =
    let algorithm =
        text.[0]
        |> Seq.map (fun c -> c = '#')
        |> Seq.toArray

    let height = text.Length - 2
    let width = text.[2].Length

    let image =
        Array.init (width * height) (fun i ->
            let x = i % width
            let y = i / width
            text.[y + 2].[x] = '#')

    { Input.algorithm = algorithm
      background = false
      width = width
      height = height
      image = image }

let optimizeInput (input: Input) = input

let enhanceImage (input: OptimInput) : OptimInput =
    let newWidth = input.width + 2
    let newHeight = input.height + 2
    let backgroundIndex = if not input.background then 0 else 511
    let newBackground = input.algorithm.[backgroundIndex]
    let newImage = Array.zeroCreate (newWidth * newHeight)
    for newY = 0 to newHeight - 1 do
        for newX = 0 to newWidth - 1 do
            let oldX = newX - 1
            let oldY = newY - 1
            let mutable shift = 0
            let mutable index = 0
            for y = 1 downto -1 do
                let y' = oldY + y
                for x = 1 downto -1 do
                    let x' = oldX + x
                    let bit = input.GetPixel x' y'
                    if bit then
                        index <- index ||| (1 <<< shift)
                    shift <- shift + 1
            newImage.[newY * newWidth + newX] <- input.algorithm.[index]

    { input with
        width = newWidth
        height = newHeight
        background = newBackground
        image = newImage }

let step1 (input: OptimInput) =
    input
    |> enhanceImage
    |> enhanceImage

let step2 (input: OptimInput) =
    let mutable image = input
    for i = 1 to 50 do
        image <- enhanceImage image
    image



let main () =
    let input = Input.parseInputLines () |> parseInput
    // printfn "Input:\n%A" input
    let input = optimizeInput input
    printfn "OptimInput:\n%O" input

    let n = 1
    let time = Time.measureN

    (fun () -> step1 input)
    |> time n
    |> printfn "***** STEP1:\n%O"

    (fun () -> step2 input)
    |> time n
    |> printfn "***** STEP2:\n%O"


main ()

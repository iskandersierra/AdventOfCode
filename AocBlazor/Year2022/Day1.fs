module Year2022.Day1

open System
open System.Text
open AoCUtils.DataModel

type ElfCarry = { calories: int [] }

type Input =
    { elves: ElfCarry [] }
    override this.ToString() =
        stringBuilder {
            "Elves ["
            this.elves.Length
            "]"

            for i = 0 to this.elves.Length - 1 do
                "- "
                i + 1
                ": "
                let elf = this.elves.[i]

                for j = 0 to elf.calories.Length - 1 do
                    elf.calories.[j]
                    " "
        }

[<RequireQualifiedAccessAttribute>]
module Input =
    let parse (text: string) =
        let elves =
            text.Split("\n\n")
            |> Array.map (fun text ->
                let calories =
                    text.Split("\n") |> Array.map Int32.Parse

                { calories = calories })

        { elves = elves }

let exercise =
    { new IExercise with
        member this.Year = 2022
        member this.Day = 1
        member this.Link = "https://adventofcode.com/2022/day/1"
        member this.Title = "Calorie Counting"
        member this.GetInputSamples() = InputSample.loadFileSamples 2022 1 }

[<AutoOpen>]
module AocTestUtils

open System
open Swensen.Unquote

open AoCUtils
open AoCUtils.DataModel

let checkSamplesContains (expectedContent: string) (expectedCount: int) (exercise: IExercise) =
    let samples = exercise.GetInputSamples()

    let demos =
        samples
        |> Seq.filter (fun sample -> sample.DisplayName.Contains(expectedContent))
        |> Seq.toArray

    test <@ demos.Length = expectedCount @>

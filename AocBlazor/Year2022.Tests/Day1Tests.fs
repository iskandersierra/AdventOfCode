module Year2022.Day1Tests

open System
open Xunit
open Swensen.Unquote

open Year2022.Day1

let ``Parse valid inputs DATA`` () : obj [] seq =
    let data (text: string) (expected: Input) = [| text :> obj; expected |]

    seq {
        yield data "1000" { elves = [| { calories = [| 1000 |] } |] }
        yield data "1000\n1001" { elves = [| { calories = [| 1000; 1001 |] } |] }

        yield
            data
                "1000\n1001\n\n2000"
                { elves =
                    [| { calories = [| 1000; 1001 |] }
                       { calories = [| 2000 |] } |] }
    }

[<Theory>]
[<MemberData(nameof ``Parse valid inputs DATA``)>]
let ``Parse valid inputs`` (text: string) (expected: Input) =
    let actual = Input.parse text

    test <@ expected = actual @>


[<Fact>]
let ``Samples contain one demo`` () =
    exercise |> checkSamplesContains "Demo" 1

[<Fact>]
let ``Samples contain one puzzle input`` () =
    exercise |> checkSamplesContains "Puzzle Input" 1

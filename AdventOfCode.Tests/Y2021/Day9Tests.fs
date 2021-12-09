module AdventOfCode.Tests.Y2021.Day9Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day9
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Example caseA``() =
    let actual =
        seq {
            "2199943210"
            "3987894921"
            "9856789892"
            "8767896789"
            "9899965678"
        }
        |> puzzle Case.A
    test <@ actual = 15 @>

[<Fact>]
let ``Example caseB``() =
    let actual =
        seq {
            "2199943210"
            "3987894921"
            "9856789892"
            "8767896789"
            "9899965678"
            }
        |> puzzle Case.B
    test <@ actual = 1134 @>
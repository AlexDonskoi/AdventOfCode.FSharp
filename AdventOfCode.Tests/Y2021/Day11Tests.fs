module AdventOfCode.Tests.Y2021.Day11Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day11
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Example caseA``() =
    let actual =
        seq {
            "5483143223"
            "2745854711"
            "5264556173"
            "6141336146"
            "6357385478"
            "4167524645"
            "2176841721"
            "6882881134"
            "4846848554"
            "5283751526"
        } |> puzzle Case.A
    test <@ actual = 1656L @>

[<Fact>]
let ``Example caseB``() =
    let src =
        seq {
            "5483143223"
            "2745854711"
            "5264556173"
            "6141336146"
            "6357385478"
            "4167524645"
            "2176841721"
            "6882881134"
            "4846848554"
            "5283751526"
        }
    let actual = puzzle Case.B src
    test <@ actual = 195 @>
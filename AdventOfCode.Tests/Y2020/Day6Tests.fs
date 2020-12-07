namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day6Tests =

    [<Fact>]
    let ``Success multiply 2 numbers within 2 items array`` () =
        let actual = Day1.run (2,2) [1; 1]
        test <@ actual = Some 1  @>
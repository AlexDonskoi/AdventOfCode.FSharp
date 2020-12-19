namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day15
open Xunit
open Swensen.Unquote
open Xunit

module Day15Tests =

    [<Theory>]
    [<InlineData(4, 0)>]
    [<InlineData(5, 3)>]
    [<InlineData(6, 3)>]
    [<InlineData(7, 1)>]
    [<InlineData(8, 0)>]
    [<InlineData(9, 4)>]
    [<InlineData(10, 0)>]
    let ``Verify case A step by step example`` (stopAt, expectedValue) =
        let actual = CaseA.run stopAt [| 0; 3; 6 |]
        test <@ actual = expectedValue @>

    [<Theory>]
    [<InlineData("0,3,6", 436)>]
    [<InlineData("1,3,2", 1)>]
    [<InlineData("2,1,3", 10)>]
    [<InlineData("1,2,3", 27)>]
    [<InlineData("2,3,1", 78)>]
    [<InlineData("3,2,1", 438)>]
    [<InlineData("3,1,2", 1836)>]

    let ``Verify case A examples`` (input, expected) =
        let actual = puzzle Case.A input
        test <@ actual = expected @>

    [<Theory(Skip="LongRunning")>]
    [<InlineData("0,3,6", 175594)>]
    [<InlineData("1,3,2", 2578)>]
    [<InlineData("2,1,3", 3544142)>]
    [<InlineData("1,2,3", 261214)>]
    [<InlineData("2,3,1", 6895259)>]
    [<InlineData("3,2,1", 18)>]
    [<InlineData("3,1,2", 362)>]

    let ``Verify case B examples`` (input, expected) =
        let actual = puzzle Case.B input
        test <@ actual = expected @>
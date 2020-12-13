namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open AdventOfCode.Cases.Y2020.Day13
open Xunit
open Swensen.Unquote

module Day13Tests =

    let input1 = split "1
N3
F7
R90
F11"

    [<Fact>]
    let ``Verify parse schedule`` () =
        let actual = parseSchedule "1,2,3,x,x,5,m"

        test <@ actual = [(1L, 0L); (2L, 1L); (3L, 2L); (5L, 5L)] @>

    [<Fact>]
    let ``Verify parse input`` () =
        let actual = parseInput ["3"; "1,x,5,m"]

        test <@ actual = (3L, [(1L, 0L); (5L, 2L)]) @>

    [<Fact>]
    let ``Verify parse wait time`` () =
        let actual = withWaitTime 9L (7L, 0)

        test <@ actual = (7L, 5L) @>

    [<Fact>]
    let ``Verify example A`` () =
        let actual = puzzle Case.A ["939"; "7,13,x,x,59,x,31,19"]

        test <@ actual = 295L @>

    [<Fact>]
    let ``Verify schedule processing`` () =
        let actual = matchNextSchedule (0L, 7L) (13L, 1L)

        test <@ actual = (77L, 91L) @>

    [<Fact>]
    let ``Verify case B example 1`` () =
        let actual = puzzle Case.B ["0"; "7,13,x,x,59,x,31,19"]

        test <@ actual = 1068781L @>

    [<Fact>]
    let ``Verify case B example 2`` () =
        let actual = puzzle Case.B ["0"; "17,x,13,19"]

        test <@ actual = 3417L @>

    [<Fact>]
    let ``Verify case B example 3`` () =
        let actual = puzzle Case.B ["0"; "67,7,59,61"]

        test <@ actual = 754018L @>

    [<Fact>]
    let ``Verify case B example 4`` () =
        let actual = puzzle Case.B ["0"; "67,7,x,59,61"]

        test <@ actual = 1261476L @>

    [<Fact>]
    let ``Verify case B example 5`` () =
        let actual = puzzle Case.B ["0"; "1789,37,47,1889"]

        test <@ actual = 1202161486L @>
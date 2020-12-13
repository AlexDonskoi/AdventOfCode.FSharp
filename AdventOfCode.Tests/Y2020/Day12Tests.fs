namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open Xunit
open AdventOfCode.Cases.Y2020.Day12
open Swensen.Unquote

module Day12Tests =
    let split (value:string) = value.Split('\n')
    let trim (value:string) = value.Trim()

    let input1 = split "F10
N3
F7
R90
F11"

    [<Fact>]
    let ``Verify parse item`` () =
        let actual = parseLine "N50"

        test <@ actual = N 50 @>

    [<Fact>]
    let ``Verify example case A`` () =
        let actual = puzzle Case.A input1
        test <@ actual = 25 @>

    [<Fact>]
    let ``Verify example case B`` () =
        let actual = puzzle Case.B input1

        test <@ actual = 286 @>

    [<Fact>]
    let ``Verify move endpoint right turns`` () =
        let actual = moveWaypoint ((0, 0), (1, 0)) (R 180)

        test <@ actual = ((0, 0), (-1,0)) @>

    [<Fact>]
    let ``Verify case B right turns`` () =
        let actual = caseB ((0, 0), (1, 0)) [R 180; F 10]

        test <@ actual = ((-10, 0), (-1,0)) @>

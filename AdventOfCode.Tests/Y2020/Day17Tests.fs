namespace AdventOfCode.Tests.Y2020

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open AdventOfCode.Cases.Y2020.Day17
open Xunit
open Swensen.Unquote

module Day17Tests =

    [<Fact>]
    let ``Verify 1-dimension neighbors`` () =
        let actual = neighbors [0]
        test <@ actual = [[-1]; [1]] @>

    [<Fact>]
    let ``Verify 2-dimension neighbors`` () =
        let actual = neighbors [0;0]
        test <@ actual = [[-1;-1]; [-1;0]; [-1;1]; [0;-1]; [0;1]; [1;-1]; [1;0]; [1;1]] @>

    [<Fact>]
    let ``Verify 0-hyper dimension parse`` () =
        let input = split "\n" "#" |> Array.toList
        let actual = parseDimension 0 input
        test <@ actual = ((1,1, 0), 0, [[0;0]]) @>

    [<Fact>]
    let ``Verify 1-hyper dimension parse`` () =
        let input = split "\n" "#" |> Array.toList
        let actual = parseDimension 1 input
        test <@ actual = ((1, 1, 1), 0, [[0;0;0]]) @>

    [<Fact>]
    let ``Verify case A example`` () =
        let input = split "\n" ".#.
..#
###"
        let actual = puzzle Case.A input
        test <@ actual = 112 @>

    [<Fact>]
    let ``Verify case B example`` () =
        let input = split "\n" ".#.
..#
###"
        let actual = puzzle Case.B input
        test <@ actual = 848 @>
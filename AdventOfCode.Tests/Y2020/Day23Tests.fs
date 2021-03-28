namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day23
open Xunit
open Swensen.Unquote

module Day23Tests =

    [<Fact>]
    let ``Verify parse`` () =
        let actual = parse "156"
        test <@ actual = [1; 5; 6] @>

    let exampleArray = "389125467" |> parse

    (*[<Fact>]
    let ``Verify case A example step by step`` () =
        let steps = [
            [2; 8; 9; 1; 5; 4; 6; 7; 3]
            [5; 4; 6; 7; 8; 9; 1; 3; 2]
            [8; 9; 1; 3; 4; 6; 7; 2; 5]
            [4; 6; 7; 9; 1; 3; 2; 5; 8]
            [1; 3; 6; 7; 9; 2; 5; 8; 4]
            [9; 3; 6; 7; 2; 5; 8; 4; 1]
            [2; 5; 8; 3; 6; 7; 4; 1; 9]
            [6; 7; 4; 1; 5; 8; 3; 9; 2]
            [5; 7; 4; 1; 8; 3; 9; 2; 6]
            [8; 3; 7; 4; 1; 9; 2; 6; 5]
        ]
        let sut = run 1
        let rec testRec steps input =
            match steps with
            | [] -> input
            | h::rest ->
                let actual = sut input
                test <@ actual = h @>
                testRec rest actual

        let actual = testRec steps exampleArray
        test <@ CaseA.result actual = "92658374" @>*)

    [<Theory>]
    [<InlineData("389125467", "54673289")>]
    [<InlineData("289154673", "32546789")>]
    [<InlineData("546789132", "34672589")>]
    [<InlineData("891346725", "32584679")>]
    [<InlineData("467913258", "36792584")>]
    [<InlineData("136792584", "93672584")>]
    [<InlineData("936725841", "92583674")>]
    [<InlineData("258367419", "58392674")>]
    [<InlineData("674158392", "83926574")>]
    [<InlineData("574183926", "92658374")>]

    [<InlineData("12345", "56789234")>]
    let ``Verify case A example step by step`` (src, expected) =
        let actual = src |> parse |> init 9 |> run 1 |> caseA
        test <@ actual = expected @>


    [<Fact>]
    let ``Verify case A example 10 moves`` () =
        let actual = exampleArray |> init 9 |> run 10 |> caseA
        test <@ actual = "92658374" @>

    [<Fact>]
    let ``Verify case A example 100 moves`` () =
        let actual = exampleArray |> init 9 |> run 100 |> caseA
        test <@ actual = "67384529" @>
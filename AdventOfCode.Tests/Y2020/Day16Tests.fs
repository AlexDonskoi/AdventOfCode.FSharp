namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open AdventOfCode.Cases.Y2020.Day16
open Xunit
open Swensen.Unquote
open Xunit

module Day16Tests =


    [<Fact>]
    let ``Verify case A example`` () =
        let input = seq {
            [
             "class: 1-3 or 5-7"
             "row: 6-11 or 33-44"
             "seat: 13-40 or 45-50"]
            [
                "your ticket:"
                "7,1,14"]
            [
                "nearby tickets:"
                "7,3,47"
                "40,4,50"
                "55,2,20"
                "38,6,12"]
        }
        let actual = puzzle Case.A input
        test <@ actual = 71L @>

    [<Fact>]
    let ``Verify case B example`` () =
        let input = seq {
            [
             "departure class: 0-1 or 4-19"
             "row: 0-5 or 8-19"
             "departure seat: 0-13 or 16-19"]
            [
                "your ticket:"
                "11,12,13"]
            [
                "nearby tickets:"
                "3,9,18"
                "15,1,5"
                "5,14,9"]
        }
        let actual = puzzle Case.B input
        test <@ actual = 1716L @>
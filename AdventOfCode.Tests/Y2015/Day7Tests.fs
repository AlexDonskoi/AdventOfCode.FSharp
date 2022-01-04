module AdventOfCode.Tests.Y2015.Day7Tests
open AdventOfCode.Cases.Infrastructure
open Xunit
open AdventOfCode.Cases.Y2015.Day7
open Swensen.Unquote

[<Fact>]
let ``Example outputs``()=
    let input = seq{
        "123 -> x"
        "456 -> y"
        "x AND y -> d"
        "x OR y -> e"
        "x LSHIFT 2 -> f"
        "y RSHIFT 2 -> g"
        "NOT x -> h"
        "NOT y -> i"
    }
    let expected = [ "d",72us; "e",507us; "f",492us; "g",114us; "h", 65412us; "i", 65079us; "x",123us; "y", 456us] |> Map

    let actual = input |> init ||> calculate
    test <@ actual = expected  @>
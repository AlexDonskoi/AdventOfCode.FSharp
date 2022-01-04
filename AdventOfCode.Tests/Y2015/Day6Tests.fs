module AdventOfCode.Tests.Y2015.Day6Tests
open AdventOfCode.Cases.Infrastructure
open Xunit
open AdventOfCode.Cases.Y2015.Day6
open Swensen.Unquote

[<Fact>]
let ``Example case A``()=
    let input = seq{
        "turn on 0,0 through 999,999"
        "toggle 0,0 through 999,0"
        "turn off 499,499 through 500,500"
    }
    let actual = puzzle Case.A input
    test <@ actual = 1000000L-1000L-4L  @>

[<Fact>]
let ``Example case B``()=
    let input = seq{
        "turn on 0,0 through 0,0"
        "toggle 0,0 through 999,999"
    }
    let actual = puzzle Case.B input
    test <@ actual = 2000001L  @>
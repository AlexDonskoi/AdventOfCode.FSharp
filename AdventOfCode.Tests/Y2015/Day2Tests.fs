module AdventOfCode.Tests.Y2015.Day2Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2015.Day2
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("2x3x4", 58)>]
[<InlineData("1x1x10", 43)>]
let ``Example case A`` source result =
    let actual = puzzle Case.A <| Seq.singleton source
    test <@ actual = result @>

[<Theory>]
[<InlineData("2x3x4", 34)>]
[<InlineData("1x1x10", 14)>]
let ``Example case B`` source result =
    let actual = puzzle Case.B <| Seq.singleton source
    test <@ actual = result @>
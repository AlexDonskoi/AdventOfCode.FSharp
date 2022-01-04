module AdventOfCode.Tests.Y2015.Day3Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2015.Day3
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData(">", 2)>]
[<InlineData("^>v<", 4)>]
[<InlineData("^v^v^v^v^v", 2)>]
let ``Example case A`` source result =
    let actual = puzzle Case.A <| source
    test <@ actual = result @>

[<Theory>]
[<InlineData("^v", 3)>]
[<InlineData("^>v<", 3)>]
[<InlineData("^v^v^v^v^v", 11)>]
let ``Example case B`` source result =
    let actual = puzzle Case.B <| source
    test <@ actual = result @>
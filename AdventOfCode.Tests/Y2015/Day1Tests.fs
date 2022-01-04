module AdventOfCode.Tests.Y2015.Day1Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2015.Day1
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData("(())", 0)>]
[<InlineData("()()", 0)>]
[<InlineData("(((", 3)>]
[<InlineData("(()(()(", 3)>]
[<InlineData("))(((((", 3)>]
[<InlineData("())", -1)>]
[<InlineData("))(", -1)>]
[<InlineData(")))", -3)>]
[<InlineData(")())())", -3)>]
let ``Example case A`` source result =
    let actual = puzzle Case.A source
    test <@ actual = result @>

[<Theory>]
[<InlineData(")", 1)>]
[<InlineData("()())", 5)>]
let ``Example case B`` source result =
    let actual = puzzle Case.B source
    test <@ actual = result @>
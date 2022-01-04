module AdventOfCode.Tests.Y2015.Day4Tests
open AdventOfCode.Cases.Infrastructure
open Xunit
open AdventOfCode.Cases.Y2015.Day4
open Swensen.Unquote

[<Theory>]
[<InlineData("abcdef", 609043)>]
[<InlineData("pqrstuv", 1048970)>]
let ``Example case a``  input expected=
    let actual = puzzle Case.A input
    test <@ actual = expected  @>
module AdventOfCode.Tests.Y2015.Day8Tests
open AdventOfCode.Cases.Infrastructure
open Xunit
open AdventOfCode.Cases.Y2015.Day8
open Swensen.Unquote

[<Theory>]
[<InlineData("", 2)>]
[<InlineData("abc", 2)>]
[<InlineData("aaa\\\"aaa", 3)>]
[<InlineData("\\x27", 5)>]
let ``Example case A`` src expected=
    let actual = src |> Seq.singleton |> puzzle Case.A
    test <@ actual = expected  @>

[<Theory>]
[<InlineData("", 4)>]
[<InlineData("abc", 4)>]
[<InlineData("aaa\\\"aaa", 6)>]
[<InlineData("\\x27", 5)>]
let ``Example case B`` src expected =
    let actual = src |> Seq.singleton |> puzzle Case.B
    test <@ actual = expected  @>
module AdventOfCode.Tests.Y2015.Day5Tests
open AdventOfCode.Cases.Infrastructure
open Xunit
open AdventOfCode.Cases.Y2015.Day5
open Swensen.Unquote

[<Theory>]
[<InlineData("ugknbfddgicrmopn", true)>]
[<InlineData("aaa", true)>]
[<InlineData("jchzalrnumimnmhp", false)>]
[<InlineData("haegwjzuvuyypxyu", false)>]
[<InlineData("dvszwmarrgswjxmb", false)>]
let ``Example case A``  input expected =
    let actual = critA input
    test <@ actual = expected  @>

[<Theory>]
[<InlineData("qjhvhtzxzqqjkmpb", true)>]
[<InlineData("uurcxstgmygtbstg", false)>]
[<InlineData("ieodomkazucvgmuy", false)>]
[<InlineData("aaa", false)>]
[<InlineData("aaaa", true)>]
[<InlineData("aab", false)>]
[<InlineData("abaa", false)>]
[<InlineData("abaaa", false)>]
let ``Example case B``  input expected =
    let actual = critB input
    test <@ actual = expected  @>
namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day18
open Xunit
open Swensen.Unquote

module Day18Tests =

    [<Theory>]
    [<InlineData("1", 1L)>]
    [<InlineData("1 + 2", 3L)>]
    [<InlineData("1 * 2", 2L)>]
    [<InlineData("1 + 2 * 3", 9L)>]
    [<InlineData("(1 * 2)", 2L)>]
    [<InlineData("((1 * 2))", 2L)>]
    [<InlineData("((1) * (2))", 2L)>]
    [<InlineData("1 + 2 * 3 + 4 * 5 + 6", 71L)>]
    [<InlineData("2 * 3 + (4 * 5)", 26L)>]
    [<InlineData("5 + (8 * 3 + 9 + 3 * 4 * 3)", 437L)>]
    [<InlineData("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 12240L)>]
    [<InlineData("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 13632L)>]
    [<InlineData("(8 * (6 * 8 + 3) * 9 * 9 * 8) * 2", 13632L)>]

    let ``Verify example`` (expression, value) =
        let actual = expression |> Seq.singleton |> puzzle Case.A
        test <@ actual = value @>


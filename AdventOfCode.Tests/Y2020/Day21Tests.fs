namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day21
open Xunit
open Swensen.Unquote

module Day21Tests =

    [<Fact>]
    let ``Verify parse food no allergens single ingredient`` () =
        let actual = parseFood "abcd"
        test <@ actual = (["abcd"],[]) @>

    [<Fact>]
    let ``Verify parse food no allergens`` () =
        let actual = parseFood "abcd efgh"
        test <@ actual = (["abcd"; "efgh"],[]) @>

    [<Fact>]
    let ``Verify parse food single allergens single ingredient`` () =
        let actual = parseFood "abcd (contains efgh)"
        test <@ actual = (["abcd"],["efgh"]) @>

    [<Fact>]
    let ``Verify parse food allergens single ingredient`` () =
        let actual = parseFood "abcd (contains efgh, ijk)"
        test <@ actual = (["abcd"],["efgh"; "ijk"]) @>

    [<Fact>]
    let ``Verify parse food`` () =
        let actual = parseFood "abcd fgh (contains ijk, lm)"
        test <@ actual = (["abcd"; "fgh"],["ijk"; "lm"]) @>

    let exampleInput = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)"

    [<Fact>]
    let ``Verify case A example`` () =
        let actual = puzzle Case.A exampleInput
        test <@ actual = "5" @>

    [<Fact>]
    let ``Verify map`` () =

        let actual = exampleInput |> parseInput |> allergenMap
        test <@ actual = [("dairy", "mxmxvkd"); ("fish", "sqjhc"); ("soy", "fvjkl")] @>

    [<Fact>]
    let ``Verify case B example`` () =
        let actual = puzzle Case.B exampleInput
        test <@ actual = "mxmxvkd,sqjhc,fvjkl" @>
module AdventOfCode.Tests.Y2022.Day3Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2022.Day3
open Xunit
open Swensen.Unquote

let src = seq {
    "vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    "ttgJtRGJQctTZtZT"
    "CrZsJsPPZsGzwwsLwLmpwMDw"}

[<Fact>]
let ``Example A``() =

    let actual = puzzle Case.A src
    test <@ actual = 157 @>


[<Fact>]
let ``Example B``() =

    let actual = puzzle Case.B src
    test <@ actual = 70 @>
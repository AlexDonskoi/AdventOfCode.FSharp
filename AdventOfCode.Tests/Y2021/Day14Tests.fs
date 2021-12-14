module AdventOfCode.Tests.Y2021.Day14Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day14
open Xunit
open Swensen.Unquote

let example = "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

[<Fact>]
let ``Example caseA``() =
    let actual = example |> puzzle Case.A
    test <@ actual = 1588 @>

[<Fact>]
let ``Example step 1``() =
    let actual = run example 1
    test <@ actual = 1 @>

[<Fact>]
let ``Example step 2``() =
    let actual = run example 2
    test <@ actual = 5 @>
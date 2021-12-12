module AdventOfCode.Tests.Y2021.Day12Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day12
open Xunit
open Swensen.Unquote

let example1 =
        seq {
            "start-A"
            "start-b"
            "A-c"
            "A-b"
            "b-d"
            "A-end"
            "b-end"
        }

let example2 = seq {
    "dc-end"
    "HN-start"
    "start-kj"
    "dc-start"
    "dc-HN"
    "LN-dc"
    "HN-end"
    "kj-sa"
    "kj-HN"
    "kj-dc"
}

let example3 = seq {
    "fs-end"
    "he-DX"
    "fs-he"
    "start-DX"
    "pj-DX"
    "end-zg"
    "zg-sl"
    "zg-pj"
    "pj-he"
    "RW-he"
    "fs-DX"
    "pj-RW"
    "zg-RW"
    "start-pj"
    "he-WI"
    "zg-he"
    "pj-fs"
    "start-RW"
}

[<Fact>]
let ``Example1 caseA``() =
    let actual = example1 |> puzzle Case.A
    test <@ actual = 10 @>

[<Fact>]
let ``Example2 caseA``() =
    let actual = example2 |> puzzle Case.A
    test <@ actual = 19 @>

[<Fact>]
let ``Example3 caseA``() =
    let actual = example3 |> puzzle Case.A
    test <@ actual = 226 @>

[<Fact>]
let ``Example1 caseB``() =
    let actual = example1 |> puzzle Case.B
    test <@ actual = 36 @>

[<Fact>]
let ``Example2 caseB``() =
    let actual = example2 |> puzzle Case.B
    test <@ actual = 103 @>

[<Fact>]
let ``Example3 caseB``() =
    let actual = example3 |> puzzle Case.B
    test <@ actual = 3509 @>
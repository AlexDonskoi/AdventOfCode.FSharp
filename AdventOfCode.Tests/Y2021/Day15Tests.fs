module AdventOfCode.Tests.Y2021.Day15Tests

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day15
open Xunit
open Swensen.Unquote

let example = seq {
    "1163751742"
    "1381373672"
    "2136511328"
    "3694931569"
    "7463417111"
    "1319128137"
    "1359912421"
    "3125421639"
    "1293138521"
    "2311944581"
}

[<Fact>]
let ``Case A 2x2``() =
    let actual =
        seq {
            "89"
            "91"
        }|> puzzle Case.A
    test <@ actual = 10 @>

[<Fact>]
let ``Case B single item``() =
    let actual =
        seq {
            "2"
        }|> puzzle Case.B
    test <@ actual = 43 @>


[<Fact>]
let ``Example caseA``() =
    let actual = example |> puzzle Case.A
    test <@ actual = 40 @>

[<Fact>]
let ``Example caseB``() =
    let actual = example |> puzzle Case.B
    test <@ actual = 315 @>


[<Fact>]
let ``Risk count``() =
    let src = array2D [[1;9];[3;4]]
    let actual = risk 0 src (0,0) |> fst
    test <@ actual = 1 @>
    let actual = risk 0 src (2,0) |> fst
    test <@ actual = 2 @>
    let actual = risk 0 src (2,2) |> fst
    test <@ actual = 3 @>
    let actual = risk 0 src (2,1) |> fst
    test <@ actual = 1 @>
    let actual = risk 0 src (2,3) |> fst
    test <@ actual = 2 @>
    let src = array2D [[1]]
    let actual = risk 2 src (5,4) |> fst
    test <@ actual = 3 @>
    let actual = risk 0 src (4,4) |> fst
    test <@ actual = 9 @>
    let actual = risk 0 src (0,4) |> fst
    test <@ actual = 5 @>

[<Fact>]
let ``Case A 3x3``() =
    let actual =
        seq {
            "833"
            "214"
            "327"
        }|> puzzle Case.A
    test <@ actual = 12 @>

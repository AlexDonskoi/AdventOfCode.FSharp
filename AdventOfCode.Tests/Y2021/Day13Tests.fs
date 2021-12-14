module AdventOfCode.Tests.Y2021.Day13Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day13
open Xunit
open Swensen.Unquote

let example = "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

[<Fact>]
let ``Example caseA``() =
    let actual = example |> puzzle Case.A |> fst
    test <@ actual = 17 @>

[<Fact>]
let ``Simple case 1``() =
    let source = "0,0
0,2

fold along y=1
"
    let actual = source |> puzzle Case.A |> fst
    test <@ actual = 1 @>

[<Fact>]
let ``Simple case 2``() =
    let source = "0,0
4,2

fold along x=2
fold along y=1
"
    let actual = source |> puzzle Case.B |> snd
    test <@ actual = "# " @>

[<Fact>]
let ``Example caseB``() =
    let actual = example |> puzzle Case.B |> snd
    test <@ actual =
"#####
#   #
#   #
#   #
#####" @>
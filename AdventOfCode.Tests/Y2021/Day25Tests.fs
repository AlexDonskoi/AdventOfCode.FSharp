module AdventOfCode.Tests.Y2021.Day25Tests


open System
open Xunit
open Swensen.Unquote
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day25

let exampleSource =
    "v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"
    |> String.split Environment.NewLine

[<Fact>]
let ``Example case A`` () =
    let actual = puzzle Case.A exampleSource
    test <@ actual = 58 @>
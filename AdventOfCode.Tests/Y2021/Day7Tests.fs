module AdventOfCode.Tests.Y2021.Day7Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day7
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Example caseA``() =
    let state = "16,1,2,0,4,2,7,1,2,14" |> puzzle Case.A
    test <@ state = 37 @>

[<Fact>]
let ``Example caseB``() =
    let state = "16,1,2,0,4,2,7,1,2,14" |> puzzle Case.B
    test <@ state = 168 @>
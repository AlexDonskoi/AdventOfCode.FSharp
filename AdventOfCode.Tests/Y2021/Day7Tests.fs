module AdventOfCode.Tests.Y2021.Day7Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day7
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Simple case``() =
    let state = "1,2,3" |> puzzle Case.A
    test <@ state = 2 @>

[<Fact>]
let ``Example caseA``() =
    let state = "16,1,2,0,4,2,7,1,2,14" |> puzzle Case.A
    test <@ state = 37 @>

[<Fact>]
let ``Example caseB``() =
    let state = "16,1,2,0,4,2,7,1,2,14" |> puzzle Case.B
    test <@ state = 168 @>

[<Fact>]
let ``Increase sequence``() =
    let state = "1,2,3,4,5,6,7,8" |> puzzle Case.A
    test <@ state = 16 @>

[<Fact>]
let ``Decrease sequence``() =
    let state = "8,7,6,5,4,3,2,1" |> puzzle Case.A
    test <@ state = 16 @>
module AdventOfCode.Tests.Y2021.Day6Tests

open AdventOfCode.Cases.Y2021.Day6
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Example tests``() =
    let state = "3,4,3,1,2" |> init
    test <@ state = [| 0L;1L;1L;2L;1L;0L;0L;0L;0L |] @>

[<Theory>]
[<InlineData(1, 5L)>]
[<InlineData(18, 26L)>]
[<InlineData(80, 5934L)>]
[<InlineData(256, 26984457539L)>]
let ``Verify example`` days expected =
    let state = "3,4,3,1,2" |> init

    let state = next days state
    let actual = state |> Array.sum
    test <@ actual = expected @>
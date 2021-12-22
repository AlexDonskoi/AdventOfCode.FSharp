module AdventOfCode.Tests.Y2021.Day21Tests


open AdventOfCode.Cases.Y2021.Day21
open Xunit
open Swensen.Unquote


[<Fact>]
let ``Example Case A details``() =
    let p1,p2, (_,rolls) = deterministicGame 4 8
    test <@ p1 = (3, 745) @>
    test <@ p2 = (10, 1000) @>
    test <@ rolls = 993 @>

[<Fact>]
let ``Example Case A``() =
    let actual = caseA 4 8
    test <@ actual = 506466 @>

[<Fact>]
let ``Example Case B``() =
    let actual = caseB 4 8
    test <@ actual = 444356092776315L @>
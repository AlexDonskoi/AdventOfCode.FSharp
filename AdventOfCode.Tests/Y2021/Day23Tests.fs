module AdventOfCode.Tests.Y2021.Day23Tests


open System
open AdventOfCode.Cases.Y2021.Day23
open Xunit
open Swensen.Unquote

let exampleAState = [B;A],[C;D],[B;C],[D;A]
let exampleBState = [B;D;D;A],[C;C;B;D],[B;B;A;C],[D;A;C;A]

[<Fact>]
let ``Simple case``() =
    let actual = exec ([B],[C],[D],[A])
    test <@ actual = 4450 @>

[<Fact>]
let ``Simple case2``() =
    let actual = exec ([D;D],[B;B],[C;C],[A;A])
    test <@ actual = 18022 @>

[<Fact>]
let ``Example Case A``() =
    let actual = exec exampleAState
    test <@ actual = 12521 @>


[<Fact>]
let ``Example Case B``() =
    let actual = exec exampleBState
    test <@ actual = 44169 @>
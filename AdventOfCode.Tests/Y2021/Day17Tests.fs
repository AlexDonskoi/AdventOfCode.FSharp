module AdventOfCode.Tests.Y2021.Day17Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day17
open Xunit
open Swensen.Unquote


[<Fact>]
let ``Parse``() =
    let actual = parse "target area: x=269..292, y=-68..-44"
    test <@ actual = ((269, 292), (-68, -44)) @>

[<Fact>]
let ``Example caseA``() =
    let actual = run Case.A (20, 30) (-10, -5)
    test <@ actual = 45 @>

[<Fact>]
let ``Example caseB``() =
    let actual = run Case.B (20, 30) (-10, -5)
    test <@ actual = 112 @>
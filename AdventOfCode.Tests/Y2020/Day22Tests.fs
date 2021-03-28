namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day22
open Xunit
open Swensen.Unquote

module Day22Tests =

    [<Fact>]
    let ``Verify parse deck`` () =
        let actual = parseDeck "1
2
14
15"
        test <@ actual = [2; 14; 15] @>

    [<Fact>]
    let ``Verify combat winner single card`` () =
        let actual = Combat.winner ([3], [2])
        test <@ actual = [3; 2] @>

    [<Fact>]
    let ``Verify combat winner 2 cards`` () =
        let actual = Combat.winner ([3; 1], [2; 4])
        test <@ actual = [4; 2; 3; 1] @>

    [<Fact>]
    let ``Verify score`` () =
        let actual = score [100; 1; 10]
        test <@ actual = 312 @>

    let exampleInput = "Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10"

    [<Fact>]
    let ``Verify example case A`` () =
        let actual = puzzle Case.A exampleInput
        test <@ actual = 306 @>

    [<Fact>]
    let ``Verify recursive combat winner single`` () =
        let actual = RecursiveCombat.winner ([2], [1])
        test <@ actual = [2; 1] @>

    [<Fact>]
    let ``Verify recursive combat avoid recursion`` () =
        let actual = RecursiveCombat.winner ([43; 19], [2; 29; 14])
        test <@ actual = [43; 19] @>

    [<Fact>]
    let ``Verify example case B`` () =

        let actual = puzzle Case.B exampleInput

        test <@ actual = 291 @>
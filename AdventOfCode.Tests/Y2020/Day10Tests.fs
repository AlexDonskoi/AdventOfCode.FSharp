namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open Xunit
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day10Tests =

    let split (value:string) = value.Split('\n')

    let input1 = split "16
10
15
5
1
11
7
19
6
12
4"

    let input2 = split "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3"


    [<Fact>]
    let ``Verify A single 1-diff`` () =
        let actual = Day10.caseA [1]

        test <@ actual = 1 @>

    [<Fact>]
    let ``Verify A 1-diff`` () =
        let actual = Day10.caseA [1;2;5;7;10]

        test <@ actual = 6 @>

    [<Fact>]
    let ``Verify B empty`` () =
        let actual = Day10.caseB []
        test <@ actual = 1 @>

    [<Fact>]
    let ``Verify B test 1`` () =
        let actual = Day10.caseB [1;2]
        test <@ actual = 2 @>

    [<Fact>]
    let ``Verify B test 2`` () =
        let actual = Day10.caseB [1;4]
        test <@ actual = 1 @>

    [<Fact>]
    let ``Verify B example 1`` () =
        let actual = Day10.puzzle Case.B input1
        test <@ actual = 8 @>

    [<Fact>]
    let ``Verify B example 2`` () =
        let actual = Day10.puzzle Case.B input2
        test <@ actual = 19208 @>

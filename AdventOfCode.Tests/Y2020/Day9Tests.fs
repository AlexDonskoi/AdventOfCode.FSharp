namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Y2020.Day9
open Swensen.Unquote

module Day9Tests =
    let countLimit _ = 1L

    [<Fact>]
    let ``Verify item`` () =
        let actual =
            LimitedFifo.create 2L countLimit
            |> LimitedFifo.pushAll [1L; 2L; 3L]
            |> CaseA.checkPairSum 5L

        test <@ actual = true @>

    [<Fact>]
    let ``Verify all not enough numbers`` () =
        let actual = CaseA.run 2 [5L; 2L]
        test <@ actual = None @>

    [<Fact>]
    let ``Verify all success`` () =
        let actual = CaseA.run 2 [5L; 2L; 7L; 9L;]
        test <@ actual = None @>

    [<Fact>]
    let ``Verify first non matched success`` () =
        let actual = CaseA.run 3 [5L; 2L; 7L; 9L; 11L; 13L; 15L]
        test <@ actual = Some 13L @>

    [<Fact>]
    let ``Verify state after push`` () =
        let actual =
            LimitedFifo.create 2L countLimit
            |> LimitedFifo.push 2L

        test <@ actual.state = Some 1L @>

    [<Fact>]
    let ``Verify state after pushAll`` () =
        let actual =
            LimitedFifo.create 2L countLimit
            |> LimitedFifo.pushAll [2L; 3L;4L;5L]

        test <@ actual.state = Some 2L @>

    [<Fact>]
    let ``Verify min/max empty`` () =
        let actual =
            CaseB.getMinMax (0L,0L) []

        test <@ actual = (0L,0L) @>

    [<Fact>]
    let ``Verify min/max`` () =
        let actual =
            CaseB.getMinMax (0L,0L) [7L; 4L; 8L;5L;6L]

        test <@ actual = (4L,8L) @>

    [<Fact>]
    let ``Verify contigous search 1`` () =
        let actual =
            LimitedFifo.create 2L id
            |> CaseB.searchContiguous 1 [2L; 3L;4L;5L]

        test <@ actual = Some [2L] @>

    [<Fact>]
    let ``Verify contigous search 2`` () =
        let actual =
            LimitedFifo.create 2L id
            |> CaseB.searchContiguous 2 [2L; 1L;1L;5L]

        test <@ actual = Some [1L;1L] @>

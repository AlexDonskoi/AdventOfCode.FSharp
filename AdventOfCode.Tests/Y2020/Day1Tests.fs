namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day1Tests =

    [<Fact>]
    let ``Success multiply 2 numbers within 2 items array`` () =
        let actual = Day1.run (2,2) [1; 1]
        test <@ actual = Some 1  @>


    [<Fact>]
    let ``Success multiply 2 numbers within 3 items array`` () =
        let actual = Day1.run (2, 5) [1; 2; 3]
        test <@ actual = Some 6 @>

    [<Fact>]
    let ``Success multiply 3 numbers within 3 items array`` () =
        let actual = Day1.run (3, 3) [1; 1; 1]
        test <@ actual = Some 1 @>

    [<Fact>]
    let ``Failed to find 3 numbers within 3 items array`` () =
        let actual = Day1.run (3, 4) [1; 1; 1]
        test <@ actual = None @>

    [<Fact>]
    let ``Verify example`` () =
        let actual = Day1.run (2, 2020) [1721; 979; 366; 299; 675; 1456]
        test <@ actual = Some 514579 @>

    [<Fact>]
    let ``Search example items`` () =
        let actual = Day1.search (2, 2020) [1721; 979; 366; 299; 675; 1456]
        test <@ actual = Some [1721; 299] @>
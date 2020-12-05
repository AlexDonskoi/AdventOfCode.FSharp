namespace AdventOfCode.Tests.Y2015

open Xunit
open AdventOfCode.Cases.Y2015
open Swensen.Unquote

module Day4Tests =

    [<Fact>]
    let ``Success empty pattern`` () =
        let actual = Day4.case [""] []
        test <@ actual = [1]  @>

    [<Fact>]
    let ``Success empty prefix match pattern`` () =
        let actual = Day4.case [""] ['c'; '8']
        test <@ actual = [2]  @>

    [<Fact>]
    let ``Success non- prefix match pattern`` () =
        let actual = Day4.case ["1"] ['c'; '2']
        test <@ actual = [2]  @>
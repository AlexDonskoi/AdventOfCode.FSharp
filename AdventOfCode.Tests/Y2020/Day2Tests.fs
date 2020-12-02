namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Modules.Y2020
open Swensen.Unquote

module Day2Tests =

    [<Fact>]
    let ``Valid 1 item with count check`` () =
        let actual = Day2.case Day2.validateCount <| seq { "1-3 a: abcde" }
        test <@ actual = 1  @>

    [<Fact>]
    let ``Valid 2 items count check`` () =
        let actual = Day2.case Day2.validateCount <| seq { "1-3 a: abcde"; "2-9 c: ccccccccc" }
        test <@ actual = 2  @>

    [<Fact>]
    let ``Valid 1 item 1 invalid  count check`` () =
        let actual = Day2.case Day2.validateCount  <| seq { "1-3 a: abcde"; "1-3 b: cdefg" }
        test <@ actual = 1  @>

    [<Fact>]
    let ``Valid 1 item with indexes check`` () =
        let actual = Day2.case Day2.validatePosition <| seq { "1-3 a: abcde" }
        test <@ actual = 1  @>

    [<Fact>]
    let ``Valid 2 items with indexes check`` () =
        let actual = Day2.case Day2.validatePosition <| seq { "1-3 a: abcde"; "3-5 c: ccccacccc" }
        test <@ actual = 2  @>

    [<Fact>]
    let ``Valid 1 item 1 with indexes check`` () =
        let actual = Day2.case Day2.validatePosition  <| seq { "1-3 a: abcde"; "1-3 b: cdefg" }
        test <@ actual = 1  @>

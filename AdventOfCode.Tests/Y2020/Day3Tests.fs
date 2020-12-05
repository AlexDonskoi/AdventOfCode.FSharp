namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day3Tests =

    [<Fact>]
    let ``Verify no trees`` () =
        let actual = Day3.run [(1,1)] <| seq { "."; "." }
        test <@ actual = 0  @>

    [<Fact>]
    let ``Verify repeat pattern with no trees`` () =
        let actual = Day3.run  [(3,1)] <| seq { "."; "." }
        test <@ actual = 0  @>

    [<Fact>]
    let ``Verify some trees`` () =
        let actual = Day3.run  [(1,1)] <| seq { "."; ".#"; "." }
        test <@ actual = 1  @>

    [<Fact>]
    let ``Verify repeat pattern with only trees`` () =
        let actual = Day3.run  [(3,1)] <| seq { "#"; "#" }
        test <@ actual = 2  @>

    [<Fact>]
    let ``Verify start position count`` () =
        let actual = Day3.run  [(1,1)] <| seq { "#"; "." }
        test <@ actual = 1  @>

    [<Fact>]
    let ``Verify end position count`` () =
        let actual = Day3.run  [(1,1)] <| seq { "."; "#"; "#"}
        test <@ actual = 2  @>

    let exampleInput = seq{
            "..##......."
            "#...#...#.."
            ".#....#..#."
            "..#.#...#.#"
            ".#...##..#."
            "..#.##....."
            ".#.#.#....#"
            ".#........#"
            "#.##...#..."
            "#...##....#"
            ".#..#...#.#"
        }

    [<Fact>]
    let ``Verify example case A`` () =

        let actual = Day3.run  [(3,1)] exampleInput
        test <@ actual = 7  @>

    [<Fact>]
    let ``Verify example case B`` () =

        let actual = Day3.run  [(1,1); (3, 1); (5, 1);(7, 1); (1, 2)] exampleInput
        test <@ actual = 336  @>

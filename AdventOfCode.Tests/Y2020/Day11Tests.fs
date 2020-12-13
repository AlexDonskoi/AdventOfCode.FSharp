namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open Xunit
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day11Tests =
    let split (value:string) = value.Split('\n')
    let trim (value:string) = value.Trim()

    let input1 = split "#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##"


    [<Fact>]
    let ``Verify parse line`` () =
        let actual = Day11.parseLine "#.L.#"

        test <@ actual = [Day11.Occupied; Day11.NotExists; Day11.Empty; Day11.NotExists; Day11.Occupied;] @>

    [<Fact>]
    let ``Verify example A`` () =
        let actual = Day11.puzzle A input1

        test <@ actual = 37 @>
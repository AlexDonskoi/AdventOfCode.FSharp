namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day20
open Xunit
open Swensen.Unquote
open Xunit.Abstractions

module Day20Tests =

    [<Fact>]
    let ``Verify parse borders`` () =
        let (actual, _) =
            "###
#..
..."
            |> parseBorders
        test <@ actual = ("###", "#..", "...", ".##") @>

    let inputA = "Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###..."

    [<Fact>]
    let ``Verify example a`` () =
        let actual = puzzle Case.A inputA
        test <@ actual = 20899048083289L @>

    [<Fact>]
    let ``Verify start tile`` () =
        let (actualTileNum, _, _) = inputA |> parse |> start
        test <@ actualTileNum = 1951L @>

    [<Fact>]
    let ``Verify last line tile`` () =
        let tiles = inputA |> parse
        let startCorner = start tiles
        let actualTileNum = ltrTileLine [startCorner] tiles |> List.last |> tileNum
        test <@ actualTileNum = 3079L @>

    [<Fact>]
    let ``Verify bottom search`` () =
        let sourceTiles = inputA |> parse
        let startCorner = start sourceTiles
        let srcTile = startCorner |> counterClockWise
        let actualTileNum = rightNeighbour srcTile sourceTiles |> Option.map tileNum

        test <@ actualTileNum = Some 2311L @>

    [<Fact>]
    let ``Verify right search`` () =
        let sourceTiles = inputA |> parse
        let startCorner = start sourceTiles
        let actualTileNum = rightNeighbour startCorner sourceTiles |> Option.map tileNum

        test <@ actualTileNum = Some 2729L @>

    [<Fact>]
    let ``Verify clockwise src`` () =
        let (_, _, actual) = clockwise (0L, ("","","",""), ["12"; "34"])
        test <@ actual = ["31";"42"] @>

    [<Fact>]
    let ``Verify clockwise borders`` () =
        let (_, actual, _) = clockwise (0L, ("ab","cd","ef","gh"), [])
        test <@ actual = ("gh", "ab","cd","ef") @>

    [<Fact>]
    let ``Verify counterclockwise src`` () =
        let (_, _, actual) = counterClockWise (0L, ("","","",""), ["12"; "34"])
        test <@ actual = ["24";"13"] @>

    [<Fact>]
    let ``Verify flipX src`` () =
        let (_, _, actual) = flipOverX (0L, ("","","",""), ["12"; "34"])
        test <@ actual = ["34";"12"] @>

    [<Fact>]
    let ``Verify flipY src`` () =
        let (_, _, actual) = flipOverY (0L, ("","","",""), ["12"; "34"])
        test <@ actual = ["21";"43"] @>

    [<Fact>]
    let ``Verify  counterclockwise borders`` () =
        let (_, actual, _) = counterClockWise (0L, ("ab","cd","ef","gh"), [])
        test <@ actual = ("cd","ef","gh", "ab") @>

    [<Fact>]
    let ``Verify example monster count`` () =
        let srcImage = inputA |> parse |> image

        let actual = CaseB.monsterCount srcImage

        test <@ actual = 2 @>

    [<Fact>]
    let ``Verify monster match`` () =
        let actual =
            [
                "...................#...."
                ".#....##....##....###..."
                "..#..#..#..#..#..#......"
            ] |> CaseB.monsterCount
        test <@ actual = 1 @>

    [<Fact>]
    let ``Verify monster top shift left`` () =
        let actual =
            [
                "..................#....."
                ".#....##....##....###..."
                "..#..#..#..#..#..#......"
            ] |> CaseB.monsterCount
        test <@ actual = 0 @>

    [<Fact>]
    let ``Verify monster top shift right`` () =
        let actual =
            [
                "....................#..."
                ".#....##....##....###..."
                "..#..#..#..#..#..#......"
            ] |> CaseB.monsterCount
        test <@ actual = 0 @>

    [<Fact>]
    let ``Verify monster mid shift left`` () =
        let actual =
            [
                "...................#...."
                "#....##....##....###...."
                "..#..#..#..#..#..#......"
            ] |> CaseB.monsterCount
        test <@ actual = 0 @>

    [<Fact>]
    let ``Verify monster mid shift right`` () =
        let actual =
            [
                "...................#...."
                "..#....##....##....###.."
                "..#..#..#..#..#..#......"
            ] |> CaseB.monsterCount
        test <@ actual = 0 @>

    [<Fact>]
    let ``Verify monster bot shift left`` () =
        let actual =
            [
                "...................#...."
                ".#....##....##....###..."
                ".#..#..#..#..#..#......."
            ] |> CaseB.monsterCount
        test <@ actual = 0 @>

    [<Fact>]
    let ``Verify monster intersect captures`` () =
        let actual =
            [
                "...#...........##.###."
                "#.#....##...###....###"
                "#..##.#..#..##.#..##.."
            ] |> CaseB.monsterCount
        test <@ actual = 1 @>


    [<Fact>]
    let ``Verify monster bot shift right`` () =
        let actual =
            [
                "...................#...."
                ".#....##....##....###..."
                "...#..#..#..#..#..#....."
            ] |> CaseB.monsterCount
        test <@ actual = 0 @>

    [<Fact>]
    let ``Verify reassemble`` () =
        let actual = inputA |> parse |> reassemble |> List.map (List.map tileNum)

        test <@ actual = [[1951L; 2729L; 2971L]; [2311L; 1427L; 1489L]; [3079L; 2473L; 1171L]] @>

    let inputExampleImage = ".#.#..#.##...#.##..#####
###....#.#....#..#......
##.##.###.#.#..######...
###.#####...#.#####.#..#
##.#....#.##.####...#.##
...########.#....#####.#
....#..#...##..#.#.###..
.####...#..#.....#......
#..#.##..#..###.#.##....
#.####..#.####.#.#.###..
###.#.#...#.######.#..##
#.####....##..########.#
##..##.#...#...#.#.#.#..
...#..#..#.#.##..###.###
.#.#....#.##.#...###.##.
###.#...#..#.##.######..
.#.#.###.##.##.#..#.##..
.####.###.#...###.#..#.#
..#.#..#..#.#.#.####.###
#..####...#.#.#.###.###.
#####..#####...###....##
#.##..#..#...#..####...#
.#.###..##..##..####.##.
...###...##...#...#..###" |> String.split "\n" |> Array.toList

    [<Fact>]
    let ``Verify reassemble image`` () =
        let rec compare = function
            | h1::rest1, h2::rest2 ->
                test <@ h1 = h2 @>
                compare (rest1, rest2)
            | _ -> ()
        let actual = inputA |> parse |> image |> imageFlipX |> imageClockwise
        let mapper i el = i, el
        compare (List.mapi mapper actual, List.mapi mapper inputExampleImage)

    [<Fact>]
    let ``Verify search monster image example`` () =
        let actual = inputExampleImage |> imageFlipX |> imageClockwise |> CaseB.monsterCount

        test <@ actual = 2 @>

    [<Fact>]
    let ``Verify example case b`` () =
        let actual = puzzle Case.B inputA
        test <@ actual = 273L @>
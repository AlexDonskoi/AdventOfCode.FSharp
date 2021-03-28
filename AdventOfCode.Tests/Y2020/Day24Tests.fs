namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day24
open Xunit
open Swensen.Unquote

module Day24Tests =

    [<Theory>]
    [<InlineData("esew", 1, -1)>]
    [<InlineData("nwwswee", 0, 0)>]
    let ``Verify reference`` (src, x ,y) =
        let actual = reference <| Seq.toList src <| (0, 0)
        test <@ actual = (x, y) @>

    let inputA = "sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew" |> String.split "\n" |> Seq.ofArray


    [<Fact>]
    let ``Verify case A example`` () =
        let actual = puzzle Case.A inputA
        test <@ actual = 10 @>

    [<Fact>]
    let ``Verify case B nextState single item`` () =
        let actual = [(0, 0), ()] |> Map.ofList |> nextState |> Map.toList
        test <@ actual = [] @>

    [<Fact>]
    let ``Verify case B nextState neighbour item item`` () =
        let actual = [(0, 0); (2, 0)] |> List.map (fun a -> a, ()) |> Map.ofList |> nextState
        test <@ Map.containsKey (0, 0) actual @>
        test <@ Map.containsKey (2, 0) actual @>
        test <@ Map.containsKey (1, -1) actual @>
        test <@ Map.containsKey (1, 1) actual @>

    [<Theory>]
    [<InlineData(0, 10)>]
    [<InlineData(1, 15)>]
    let ``Verify case B day state`` (day, count) =
        let actual = inputA |> initState |> dayState day
        test <@ Map.count actual = count @>

    [<Fact>]
    let ``Verify case b example step by step`` () =
        let steps =
            [
                0, 10
                1, 15
                2, 12
                3, 25
                4, 14
                5, 23
                6, 28
                7, 41
                8, 37
                9, 49
                10, 37
                20, 132
                30, 259
                40, 406
                50, 566
                60, 788
                70, 1106
                80, 1373
                90, 1844
                100, 2208
            ]

        let rec testRec stepNum steps input =
            match steps with
            | [] -> ()
            | (num, count)::rest when num <= stepNum ->
                test <@ count = Map.count input @>
                testRec (stepNum + 1) rest (nextState input)
            | _ -> testRec (stepNum + 1) steps (nextState input)

        inputA |> initState |> testRec 0 steps
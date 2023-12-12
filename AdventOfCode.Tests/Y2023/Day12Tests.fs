module AdventOfCode.Tests.Y2023.Day12Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2023.Day12
open AutoFixture.Xunit2
open Xunit
open Swensen.Unquote



[<Fact>]
let ``Groups test 1``() =

    let actual = groups [0] 1L
    test <@ actual = [1] @>
    
[<Fact>]
let ``Groups test 2``() =

    let actual = groups [0] 3L
    test <@ actual = [2] @>
    
[<Fact>]
let ``Groups test 3``() =

    let actual = groups [0] 5L
    test <@ actual = [1; 1] @>
    
[<Fact>]
let ``Mask test 1``() =

    let mask = "???" |> Seq.toList
    for i in 0L..7L do
        let actual = maskMatch i mask
        test <@ actual @>
        
[<Fact>]
let ``Mask test 2``() =

    let mask = "?#?" |> Seq.toList
    for i in 0L..7L do
        let actual = maskMatch i mask
        let expected = i &&& 2L = 2L
        test <@ actual = expected @>

[<Fact>]
let ``Mask example 2``() =

    let mask = "???.###" |> Seq.toList
    let actual = maskMatch 117L mask
    test <@ actual @>
    
[<Fact>]
let ``Mask example 3``() =

    let mask = "????.######..#####." |> Seq.toList
    let actual = maskMatch 255976L mask
    test <@ actual @>
                
[<Theory>]
[<InlineData("??.# 1,1", 2L)>]
[<InlineData("???.### 1,1,3", 1L)>]
[<InlineData(".??..??...?##. 1,1,3", 4L)>]
[<InlineData("?#?#?#?#?#?#?#? 1,3,1,6", 1L)>]
[<InlineData("????.#...#... 4,1,1", 1L)>]
[<InlineData("????.######..#####. 1,6,5", 4L)>]
[<InlineData("?###???????? 3,2,1", 10L)>]
let ``Puzzle example 1``(src, expected) =

    let src =  Seq.singleton src 
    let actual = puzzle Case.A src
    test <@ actual = expected @>        
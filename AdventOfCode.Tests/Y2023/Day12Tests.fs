module AdventOfCode.Tests.Y2023.Day12Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2023.Day12
open AutoFixture.Xunit2
open Xunit
open Swensen.Unquote
    
[<Fact>]
let ``merge test 1``() =

    let actual = merge (No, [1; 1]) (No, [1; 1])
    test <@ actual = (No, [1; 1; 1; 1]) @>
    
[<Fact>]
let ``merge test 2``() =

    let actual = merge (No, [1; 1]) (Left, [1; 1])
    test <@ actual = (No, [1; 1; 1; 1]) @>
    
[<Fact>]
let ``merge test 3``() =

    let actual = merge (No, [1; 1]) (Both, [1;1])
    test <@ actual = (Right, [1; 1; 1; 1]) @>
    
    
[<Fact>]
let ``merge test 4``() =

    let actual = merge (Right, [1;1]) (Both, [1;1])
    test <@ actual = (Right, [1; 2; 1]) @>
    
[<Fact>]
let ``merge test 5``() =

    let actual = merge (Right, [1;1]) (Left, [1;1])
    test <@ actual = (No, [1; 2; 1]) @>
                    
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
    
[<Theory>]
[<InlineData("? 1", 1L)>]
[<InlineData("???.### 1,1,3", 1L)>]
[<InlineData(".??..??...?##. 1,1,3", 16384L)>]
[<InlineData("?#?#?#?#?#?#?#? 1,3,1,6", 1L)>]
[<InlineData("????.#...#... 4,1,1", 16L)>]
[<InlineData("????.######..#####. 1,6,5", 2500L)>]
[<InlineData("?###???????? 3,2,1", 506250L)>]
let ``Puzzle example 2``(src, expected) =

    let src =  Seq.singleton src 
    let actual = puzzle Case.B src
    test <@ actual = expected @>           
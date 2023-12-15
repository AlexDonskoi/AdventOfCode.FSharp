module AdventOfCode.Tests.Y2023.Day12Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2023.Day12
open AutoFixture.Xunit2
open Xunit
open Swensen.Unquote



[<Fact>]
let ``isSubPattern test 1``() =

    let actual = isSubPattern [|1; 1|] (No, [|1|])
    test <@ actual @>
    
[<Fact>]
let ``isSubPattern test 2``() =

    let actual = isSubPattern [|1; 1|] (No, [|1; 1|])
    test <@ actual @>    
 
[<Fact>]
let ``isSubPattern test 3``() =

    let actual = isSubPattern [|1; 1|] (No, [|1; 1; 1|])
    test <@ actual |> not @>  
       
[<Fact>]
let ``isSubPattern test 4``() =

    let actual = isSubPattern [|1; 2|] (No, [|1; 1|])
    test <@ actual |> not @>
    
[<Fact>]
let ``isSubPattern test 5``() =

    let actual = isSubPattern [|1; 2|] (Right, [|1; 1|])
    test <@ actual @>
    
[<Fact>]
let ``merge test 1``() =

    let actual = merge ((No, [|1; 1|]), 2) ((No, [|1; 1|]), 3)
    test <@ actual = ((No, [|1; 1; 1; 1|]), 6) @>
    
[<Fact>]
let ``merge test 2``() =

    let actual = merge ((No, [|1; 1|]), 2) ((Left, [|1; 1|]), 3)
    test <@ actual = ((No, [|1; 1; 1; 1|]), 6) @>
    
[<Fact>]
let ``merge test 3``() =

    let actual = merge ((No, [|1; 1|]), 2) ((Both, [|1;1|]), 3)
    test <@ actual = ((Right, [|1; 1; 1; 1|]), 6) @>
    
    
[<Fact>]
let ``merge test 4``() =

    let actual = merge ((Right, [|1;1|]), 2) ((Both, [|1;1|]), 3)
    test <@ actual = ((Right, [|1; 2; 1|]), 6) @>
    
[<Fact>]
let ``merge test 5``() =

    let actual = merge ((Right, [|1;1|]), 2) ((Left, [|1;1|]), 3)
    test <@ actual = ((No, [|1; 2; 1|]), 6) @>
    
[<Fact>]
let ``Get options 1``() =

    let actual = options [| '#'; '#'; '#'|] [|3|] initMap 0
    let expected = Map [(No, [|3|]), 1L] 
    test <@ actual = expected @> 
                
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
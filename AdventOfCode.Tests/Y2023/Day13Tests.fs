module AdventOfCode.Tests.Y2023.Day13Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2023.Day13
open AutoFixture.Xunit2
open FSharp.Collections
open Xunit
open Swensen.Unquote

let toArray src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 '.'
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v]] |> ignore
    map

[<Fact>]
let ``checkSumm test 1``() =

    let tile =
        seq {
            "12"
            "12"
        }
        |> toArray
        
    let actual = checkSymm 0 tile
    test <@ actual = Row 1 @>
    
[<Fact>]
let ``checkSumm test 2``() =

    let tile =
        seq {
            "11"
            "22"
        }
        |> toArray
        
    let actual = checkSymm 0 tile
    test <@ actual = Col 1 @>

[<Fact>]
let ``checkSumm Example 1``() =

    let tile =
        seq {
            "#.##..##."
            "..#.##.#."
            "##......#"
            "##......#"
            "..#.##.#."
            "..##..##."
            "#.#.##.#."
        }
        |> toArray
        
    let actual = checkSymm 0 tile
    test <@ actual = Col 5 @>        
    
[<Fact>]
let ``checkSumm example 2``() =

    let tile =
        seq {
            "#...##..#"
            "#....#..#"
            "..##..###"
            "#####.##."
            "#####.##."
            "..##..###"
            "#....#..#"
        }
        |> toArray
        
    let actual = checkSymm 0 tile
    test <@ actual = Row 4 @>
    
[<Fact>]
let ``checkSumm Example 1 B``() =

    let tile =
        seq {
            "#.##..##."
            "..#.##.#."
            "##......#"
            "##......#"
            "..#.##.#."
            "..##..##."
            "#.#.##.#."
        }
        |> toArray
        
    let actual = checkSymm 1 tile
    test <@ actual = Col 5 @>        
    
[<Fact>]
let ``checkSumm example 2 B``() =

    let tile =
        seq {
            "#...##..#"
            "#....#..#"
            "..##..###"
            "#####.##."
            "#####.##."
            "..##..###"
            "#....#..#"
        }
        |> toArray
        
    let actual = checkSymm 0 tile
    test <@ actual = Row 4 @>    
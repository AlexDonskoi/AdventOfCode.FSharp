module AdventOfCode.Tests.Y2021.Day18Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day18
open Xunit
open Swensen.Unquote

let magnitude = magnitudeRec 4 []
let sum = Seq.map parse >> Seq.reduce add


[<Fact>]
let ``Explode example 1``() =
    let actual = "[[[[[9,8],1],2],3],4]" |> parse |> explode id []
    let expected = "[[[[0,9],2],3],4]" |> parse
    test <@ actual = expected @>


[<Fact>]
let ``Explode example 2``() =
    let actual = "[7,[6,[5,[4,[3,2]]]]]" |> parse |> explode id []
    let expected = "[7,[6,[5,[7,0]]]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Explode example 3``() =
    let actual = "[[6,[5,[4,[3,2]]]],1]" |> parse |> explode id []
    let expected = "[[6,[5,[7,0]]],3]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Explode example 4``() =
    let actual = "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]" |> parse |> explode id []
    let expected = "[[3,[2,[8,0]]],[9,[5,[7,0]]]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Split 1``() =
    let actual = [(10,1);(0,1)] |> split []
    test <@ actual = [(5,2);(5,2);(0,1)] @>
    let actual = [(11,1);(0,1)] |> split []
    test <@ actual = [(5,2);(6,2);(0,1)] @>

[<Fact>]
let ``Add 1``() =
    let v1 = "[[[[4,3],4],4],[7,[[8,4],9]]]" |> parse
    let v2 = "[1,1]" |> parse
    let actual = add v1 v2
    let expected = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Add step 3``() =
    let v1 = "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]" |> parse
    let v2 = "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]" |> parse
    let actual = add v1 v2

    let expected = "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Add step 4``() =
    let v1 = "[[[[7,0],[7,7]],[[7,7],[7,8]]],[[[7,7],[8,8]],[[7,7],[8,7]]]]" |> parse
    let v2 = "[7,[5,[[3,8],[1,4]]]]" |> parse
    let actual = add v1 v2

    let expected = "[[[[7,7],[7,8]],[[9,5],[8,7]]],[[[6,8],[0,8]],[[9,9],[9,0]]]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Sum 1 to 4``() =
    let actual =
        seq {
            "[1,1]"
            "[2,2]"
            "[3,3]"
            "[4,4]"
        }|> sum

    let expected = "[[[[1,1],[2,2]],[3,3]],[4,4]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Sum 1 to 5``() =
    let actual =
        seq {
            "[1,1]"
            "[2,2]"
            "[3,3]"
            "[4,4]"
            "[5,5]"
        }|> sum


    let expected = "[[[[3,0],[5,3]],[4,4]],[5,5]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Sum 1 to 6``() =
    let actual =
        seq {
            "[1,1]"
            "[2,2]"
            "[3,3]"
            "[4,4]"
            "[5,5]"
            "[6,6]"
        } |> sum


    let expected = "[[[[5,0],[7,4]],[5,5]],[6,6]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Sum example 1``() =
    let actual =
        seq {
            "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
            "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
            "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
            "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
            "[7,[5,[[3,8],[1,4]]]]"
            "[[2,[2,2]],[8,[8,1]]]"
            "[2,9]"
            "[1,[[[9,3],9],[[9,0],[0,7]]]]"
            "[[[5,[7,4]],7],1]"
            "[[[[4,2],2],6],[8,7]]"
        }|> sum
    let expected = "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]" |> parse
    test <@ actual = expected @>

[<Fact>]
let ``Sum example 2``() =
    let actual =
        seq {
            "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
            "[[[5,[2,8]],4],[5,[[9,9],0]]]"
            "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
            "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
            "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
            "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
            "[[[[5,4],[7,7]],8],[[8,3],8]]"
            "[[9,3],[[9,9],[6,[4,9]]]]"
            "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
            "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
        }|> sum
    let expected = "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]" |> parse
    test <@ actual = expected @>

[<Theory>]
[<InlineData("[9,1]", 29L)>]
[<InlineData("[1,9]", 21L)>]
[<InlineData("[[9,1],[1,9]]", 129L)>]
[<InlineData("[[1,2],[[3,4],5]]", 143L)>]
[<InlineData("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]", 1384L)>]
[<InlineData("[[[[1,1],[2,2]],[3,3]],[4,4]]", 445L)>]
[<InlineData("[[[[3,0],[5,3]],[4,4]],[5,5]]", 791L)>]
[<InlineData("[[[[5,0],[7,4]],[5,5]],[6,6]]", 1137L)>]
[<InlineData("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]", 3488L)>]
[<InlineData("[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]", 4140L)>]
let ``Magnitude 1`` src expected =
    let actual = seq { yield src } |> puzzle Case.A
    test <@ actual = expected @>

[<Fact>]
let ``Max magnitude``() =
    let actual =
        seq {
            "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
            "[[[5,[2,8]],4],[5,[[9,9],0]]]"
            "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
            "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
            "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
            "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
            "[[[[5,4],[7,7]],8],[[8,3],8]]"
            "[[9,3],[[9,9],[6,[4,9]]]]"
            "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
            "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
        }|> puzzle Case.B
    test <@ actual = 3993 @>
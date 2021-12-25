module AdventOfCode.Tests.Y2021.Day24Tests


open System
open AdventOfCode.Cases.Y2021.Day24
open Xunit
open Swensen.Unquote

[<Theory>]
[<InlineData(10)>]
let ``Negative int in x`` var =
    let source =
        seq {
            "inp x"
            "mul x -1"
        } |> commands
    let actual = exec [|0;0;0;0|] [var] source
    test <@ actual = Some [|-var;0;0;0|] @>

[<Theory>]
[<InlineData(5, 15, 1)>]
[<InlineData(4, 15, 0)>]
let ``Check z = 3*x`` f s expected =
    let source =
        seq {
            "inp z"
            "inp x"
            "mul z 3"
            "eql z x"
        } |> commands
    let actual = exec [|0;0;0;0|] [f;s] source
    test <@ actual = Some [|s;0;expected;0|] @>

[<Fact>]
let ``Decrement 1``() =
    let actual = decrement [|9;1;1;1|] 3
    test <@ actual = Some [|8;9;9;9|] @>


[<Fact>]
let ``Decrement lowest``() =
    let actual = decrement [|1;1;1;1|] 3
    test <@ actual = None @>

[<Fact>]
let ``Max``() =
    let actual = result [| 5;1;9;8;3;9;9;9;9;4;7;9;9;9 |]
    test <@ actual = 0 @>

[<Fact>]
let ``Min``() =
    let actual = result [| 1;1;2;1;1;7;9;1;1;1;1;3;6;5 |]
    test <@ actual = 0 @>
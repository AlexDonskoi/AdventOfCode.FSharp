module AdventOfCode.Tests.Y2022.Day13Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day18
open AdventOfCode.Cases.Y2022.Day13
open Xunit
open Swensen.Unquote


[<Fact>]
let ``test 1``() =
    let actual = "[]" |> Seq.toList |> parse <| [] |> Seq.head
    test <@ actual = Group (List []) @>
    
    
[<Fact>]
let ``test 2``() =
    let actual = "[[],[]]" |> Seq.toList |> parse <| [] |> Seq.head
    test <@ actual = Group (List [(List []); (List [])]) @>
    
[<Fact>]
let ``test 3``() =
    let actual = "[10,11,[]]" |> Seq.toList |> parse <| [] 
    test <@ actual = [Group (List [(Value 10); Value 11; (List [])])]@>
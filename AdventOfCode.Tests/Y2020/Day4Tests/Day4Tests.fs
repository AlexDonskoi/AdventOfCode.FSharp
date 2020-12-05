namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day4Tests =

    [<Fact>]
    let ``Check valid empty token`` () =
        let actual = Day4.token "a:"
        test <@ actual = KeyValue ("a", "")  @>

    [<Fact>]
    let ``Check valid Token`` () =
        let actual = Day4.token "a:b"
        test <@ actual = KeyValue ("a", "b")  @>

    [<Fact>]
    let ``Check invalid Token`` () =
        let actual = Day4.token "ab"
        test <@ actual = Value "ab"  @>

    [<Fact>]
    let ``Check line tokens`` () =
        let actual = Day4.tokens "a: c:d e"
        test <@ actual = [KeyValue ("a",""); KeyValue("c", "d"); Value "e"]  @>

    [<Fact>]
    let ``Check multiline tokens`` () =
        let actual = seq { "a:"; "b"; ""; "c:" } |> Day4.records |> Seq.toList
        let expected = [
            [KeyValue ("a",""); Value("b")]
            [KeyValue ("c","")]
        ]
        test <@ actual = expected  @>

    [<Fact>]
    let ``Verify isValid single`` () =
        let validTokens = ["a"]
        let checkToken = seq { KeyValue("a", "") }
        let actual = Day4.hasAll validTokens checkToken
        test <@ actual = true  @>

    [<Fact>]
    let ``Verify isValid multiple`` () =
        let validTokens =  ["a"; "b" ]
        let checkToken = seq { KeyValue("a", ""); KeyValue("b", "") }
        let actual = Day4.hasAll validTokens checkToken
        test <@ actual = true  @>

    [<Fact>]
    let ``Verify isValid single missed`` () =
        let validTokens = ["a"; "c" ]
        let checkToken = seq { KeyValue("a", ""); KeyValue("b", "") }
        let actual = Day4.hasAll validTokens checkToken
        test <@ actual = false  @>

    [<Fact>]
    let ``Verify records`` () =
        let expected = [
            [KeyValue("a", ""); KeyValue("d", "d");KeyValue("b", ""); KeyValue("c", "c")]
            [KeyValue("c", "")]
        ]
        let actual = ["a: d:d"; "b: c:c"; ""; ""; "c:"] |> Day4.records |> Seq.toList
        test <@ actual = expected  @>


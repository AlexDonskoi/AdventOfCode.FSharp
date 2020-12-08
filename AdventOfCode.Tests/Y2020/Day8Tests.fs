namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day8Tests =

    let split (value:string) = value.Split('\n')

    [<Fact>]
    let ``Parse line`` () =
        let actual = Day8.parse "jmp +1"
        let expected = Day8.Jmp  1
        test <@ actual = expected @>

    [<Fact>]
    let ``Verify unknown instruction`` () =
        let actual =
            "noop"
            |> split
            |> Array.toSeq
            |> Day8.puzzle A
        test <@ actual = Day8.Fail "noop" @>

    [<Fact>]
    let ``Verify out of lange jump instruction`` () =
        let actual =
            "jmp -1"
            |> split
            |> Array.toSeq
            |> Day8.puzzle A
        test <@ actual = Day8.Fail "incorrect index" @>

    [<Fact>]
    let ``Verify out of range exit instruction`` () =
        let actual =
            "acc +2
jmp 3
acc -1"
            |> split
            |> Array.toSeq
            |> Day8.puzzle A
        test <@ actual = Day8.Exit 2 @>

    [<Fact>]
    let ``Verify example A`` () =
        let actual =
            "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
            |> split
            |> Array.toSeq
            |> Day8.puzzle A
        test <@ actual = Day8.Loop 5 @>

    [<Fact>]
    let ``Verify example B`` () =
        let actual =
            "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6"
            |> split
            |> Array.toSeq
            |> Day8.puzzle B
        test <@ actual = Day8.Exit 8 @>


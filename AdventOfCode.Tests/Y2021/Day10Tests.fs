module AdventOfCode.Tests.Y2021.Day10Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day10
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Example caseA``() =
    let actual =
        seq {
            "[({(<(())[]>[[{[]{<()<>>"
            "[(()[<>])]({[<{<<[]>>("
            "{([(<{}[<>[]}>{[]{[(<()>"
            "(((({<>}<{<{<>}{[]{[]{}"
            "[[<[([]))<([[{}[[()]]]"
            "[{[{({}]{}}([{[{{{}}([]"
            "{<[[]]>}<{[{[{[]{()[[[]"
            "[<(<(<(<{}))><([]([]()"
            "<{([([[(<>()){}]>(<<{{"
            "<{([{{}}[<[[[<>{}]]]>[]]"
        }
        |> puzzle Case.A
    test <@ actual = 26397 @>

[<Theory>]
[<InlineData("}}]])})]", 288957)>]
[<InlineData(")}>]})", 5566)>]
[<InlineData("}}>}>))))", 1480781)>]
[<InlineData("]]}}]}]}>", 995444)>]
[<InlineData("])}>", 294)>]
let ``IncompleteScore`` input score =
    let actual = input |> Seq.toList |> Incomplete |> incompleteScore
    test <@ actual = score @>

(*let ``Incomplete scope`` input expected =
    let actual =
        seq {
            "[({(<(())[]>[[{[]{<()<>>"
            "[(()[<>])]({[<{<<[]>>("
            "{([(<{}[<>[]}>{[]{[(<()>"
            "(((({<>}<{<{<>}{[]{[]{}"
            "[[<[([]))<([[{}[[()]]]"
            "[{[{({}]{}}([{[{{{}}([]"
            "{<[[]]>}<{[{[{[]{()[[[]"
            "[<(<(<(<{}))><([]([]()"
            "<{([([[(<>()){}]>(<<{{"
            "<{([{{}}[<[[[<>{}]]]>[]]"
        }
        |> Seq.
    test <@ actual = 26397 @>*)

[<Fact>]
let ``Example caseB``() =
    let actual =
        seq {
            "[({(<(())[]>[[{[]{<()<>>"
            "[(()[<>])]({[<{<<[]>>("
            "{([(<{}[<>[]}>{[]{[(<()>"
            "(((({<>}<{<{<>}{[]{[]{}"
            "[[<[([]))<([[{}[[()]]]"
            "[{[{({}]{}}([{[{{{}}([]"
            "{<[[]]>}<{[{[{[]{()[[[]"
            "[<(<(<(<{}))><([]([]()"
            "<{([([[(<>()){}]>(<<{{"
            "<{([{{}}[<[[[<>{}]]]>[]]"
            }
        |> puzzle Case.B
    test <@ actual = 288957 @>
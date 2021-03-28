namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day19
open Xunit
open Swensen.Unquote

module Day19Tests =

    [<Fact>]
    let ``Verify matched int replacement`` () =
        let actual = replaceRule <| Map.ofList [1, "repl"] <| "1"
        test <@ actual = "repl" @>

    [<Fact>]
    let ``Verify non-matched int ignore replacement`` () =
        let runner = replaceRule <| Map.ofList [1, "repl"]
        raises <@ runner "2" @>

    [<Fact>]
    let ``Verify NaN ignore replacement`` () =
        let actual = replaceRule <| Map.ofList [1, "repl"] <| "repl2"
        test <@ actual = "repl2" @>

    [<Theory>]
    [<InlineData("0: \"a\"\n\na", 1)>]
    [<InlineData("0: \"a\"\n\na\nb", 1)>]
    let ``Verify case A test`` (input, expected) =

        let actual = puzzle Case.A input
        test <@ actual = expected @>

    let exampleCaseAInput = "0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: \"a\"
5: \"b\""

    [<Fact>]
    let ``Verify example A`` () =

        let srcData = "ababbb
bababa
abbbab
aaabbb
aaaabbb"

        let actual = CaseA.run exampleCaseAInput srcData
        test <@ actual = 2 @>

    let exampleCaseBInput = "42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: \"a\"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: \"b\"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1"

    let dataCaseB = "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"

    [<Fact>]
    let ``Verify example B logic A`` () =

        let actual = CaseA.run exampleCaseBInput dataCaseB
        test <@ actual = 3 @>

    [<Fact>]
    let ``Verify example B`` () =

        let actual = CaseB.run exampleCaseBInput dataCaseB
        test <@ actual = 12 @>
namespace AdventOfCode.Tests.Y2015

open Xunit
open Xunit.Abstractions
open AdventOfCode.Modules.Y2015

type Day4Tests(output:ITestOutputHelper) =
    let write result =
        output.WriteLine (sprintf "The actual result was %O" result)

    let case_result_should_match prefix pattern ind =
        let result = Day4.case [prefix] pattern
        write result
        for res in result do
            Assert.Equal(ind, res)

    [<Fact>]
    let case_should_match_empty_pattern() =
        case_result_should_match "" [] 1

    [<Fact>]
    let case_should_match_pattern() =
        case_result_should_match "" ['c'; '8'] 2

    [<Fact>]
    let case_should_use_include_prefix() =
        case_result_should_match "1" ['c'; '2'] 2
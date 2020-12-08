namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day7Tests =

    let split (value:string) = value.Split('\n')

    [<Fact>]
    let ``Parse line`` () =
        let actual =
            "light red bags contain 1 bright white bag, 2 muted yellow bags."
            |> Day7.parse
        let expected =
            "light red", [
                "bright white", 1
                "muted yellow", 2
            ]
        test <@ actual = expected @>



    [<Fact>]
    let ``Handle deepSearch`` () =
        let actual =
            [
                "key4", []
                "key3", ["key4", 1]
                "key1", ["key2", 2; "key3", 3]
                "key2", ["key4", 4]
            ]
            |> Day7.linksSearch <| [("key1", 1)] <| []

        let expectedMaps =[]
        test <@ actual = [] @>

    [<Fact>]
    let ``Verify example A`` () =
        let actual =
            "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
            |> split
            |> Array.toSeq
            |> Day7.puzzle A
        test <@ actual = 4 @>

    [<Fact>]
    let ``Verify example B`` () =
        let actual =
            "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."
            |> split
            |> Array.toSeq
            |> Day7.puzzle B
        test <@ actual = 126 @>

    [<Fact>]
    let ``Verify example B0`` () =
        let actual =
            "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags."
            |> split
            |> Array.toSeq
            |> Day7.puzzle B
        test <@ actual = 32 @>
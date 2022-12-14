module AdventOfCode.Tests.Y2022.Day11Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2022.Day11
open Xunit
open Swensen.Unquote

let source = @"|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"


[<Fact>]
let ``Example A round 20 result``() =

    let source = parse source
    let len = Seq.length source
    let actual= runA source 20
    test <@ actual = [|101; 95; 7; 105|] @>

let realInput = @"Monkey 0:
  Starting items: 89, 84, 88, 78, 70
  Operation: new = old * 5
  Test: divisible by 7
    If true: throw to monkey 6
    If false: throw to monkey 7

Monkey 1:
  Starting items: 76, 62, 61, 54, 69, 60, 85
  Operation: new = old + 1
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 6

Monkey 2:
  Starting items: 83, 89, 53
  Operation: new = old + 8
  Test: divisible by 11
    If true: throw to monkey 5
    If false: throw to monkey 3

Monkey 3:
  Starting items: 95, 94, 85, 57
  Operation: new = old + 4
  Test: divisible by 13
    If true: throw to monkey 0
    If false: throw to monkey 1

Monkey 4:
  Starting items: 82, 98
  Operation: new = old + 7
  Test: divisible by 19
    If true: throw to monkey 5
    If false: throw to monkey 2

Monkey 5:
  Starting items: 69
  Operation: new = old + 2
  Test: divisible by 2
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 6:
  Starting items: 82, 70, 58, 87, 59, 99, 92, 65
  Operation: new = old * 11
  Test: divisible by 5
    If true: throw to monkey 7
    If false: throw to monkey 4

Monkey 7:
  Starting items: 91, 53, 96, 98, 68, 82
  Operation: new = old * old
  Test: divisible by 3
    If true: throw to monkey 4
    If false: throw to monkey 2"

[<Fact>]
let ``Real round 1``() =

    let source = parse realInput
    let len = Seq.length source
    let actual = runB source 10000
    test <@ actual = [|9744L; 116196L; 121101L; 120866L; 112614L; 1838L; 114818L; 11613L|] @>
module AdventOfCode.Tests.Y2022.Day1Tests

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
    

// [<Fact>]
// let ``Example A round 20 result``() =
//     
//     let source = parse source
//     let len = Seq.length source
//     let actual= roundA source <| Array.create len 0 <| 20 <| 0
//     test <@ actual = [|101; 95; 7; 105|] @>
//
//
//
// [<Fact>]
// let ``Example A round 1``() =
//     
//     let source = parse source
//     let len = Seq.length source
//     roundA source <| Array.create len 0 <| 1 <| 0 |> ignore
//     let actual = source |> Seq.map fst |> Seq.toList
//     test <@ actual = [[20; 23; 27; 26]; [2080; 25; 167; 207; 401; 1046];[];[]] @>
//     
// [<Fact>]
// let ``Example A round 2``() =
//     
//     let source = parse source
//     let len = Seq.length source
//     roundA source <| Array.create len 0 <| 2 <| 0 |> ignore
//     let actual = source |> Seq.map fst |> Seq.toList
//     test <@ actual = [[695; 10; 71; 135;350]; [43; 49; 58; 55; 362];[];[]] @>
//
// [<Fact>]
// let ``Example A round 3``() =
//     
//     let source = parse source
//     let len = Seq.length source
//     roundA source <| Array.create len 0 <| 3 <| 0 |> ignore
//     let actual = source |> Seq.map fst |> Seq.toList
//     test <@ actual = [[16; 18; 21; 20; 122]; [1468; 22; 150; 286; 739];[];[]] @>
//     
// [<Fact>]
// let ``Example A round 20``() =
//     
//     let source = parse source
//     let len = Seq.length source
//     roundA source <| Array.create len 0 <| 20 <| 0 |> ignore
//     let actual = source |> Seq.map fst |> Seq.toList
//     test <@ actual = [[10; 12; 14; 26; 34]; [245; 93; 53; 199; 115];[];[]] @>
    
    
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
    
// [<Fact>]
// let ``Real round 1``() =
//     
//     let source = parse realInput
//     let len = Seq.length source
//     let actV = roundA source <| Array.create len 0 <| 1 <| 0 |> Seq.sum
//     let actual = source |> Seq.map fst |> Seq.toList
//     test <@ actual = [[10; 12; 14; 26; 34]; [245; 93; 53; 199; 115];[];[]] @>
//     test <@ actV =  1@>
module AdventOfCode.Tests.Y2021.Day16Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day16
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Example literal packet``() =
    let actual = "D2FE28" |> parse
    test <@ actual = [Literal (6, 4, 2021L)] @>
    let versions = actual |> versions |> List.sum
    test <@ versions = 6 @>

[<Fact>]
let ``Example 2 subpackets``() =
    let actual = "38006F45291200" |> parse
    test <@ actual = [Operator (1, 6, [Literal (6, 4, 10L); Literal (2, 4, 20L)])] @>
    let versions = actual |> versions |> List.sum
    test <@ versions = 9 @>

[<Fact>]
let ``Example 3 subpackets``() =
    let actual = "EE00D40C823060" |> parse
    test <@ actual = [Operator (7, 3, [Literal (2, 4, 1L); Literal (4, 4, 2L); Literal (1, 4, 3L)])] @>
    let versions = actual |> versions |> List.sum
    test <@ versions = 14 @>

[<Fact>]
let ``Example1 case A``() =
    let actual = "8A004A801A8002F478" |> puzzle Case.A
    test <@ actual = 16 @>

[<Fact>]
let ``Example2 case A``() =
    let actual = "620080001611562C8802118E34" |> puzzle Case.A
    test <@ actual = 12 @>

[<Fact>]
let ``Example3 case A``() =
    let actual = "C0015000016115A2E0802F182340" |> puzzle Case.A
    test <@ actual = 23 @>

[<Fact>]
let ``Example4 case A``() =
    let actual = "A0016C880162017C3686B18A3D4780" |> puzzle Case.A
    test <@ actual = 31 @>

[<Fact>]
let ``Example1 case B``() =
    let actual = "C200B40A82" |> puzzle Case.B
    test <@ actual = 3 @>

[<Fact>]
let ``Example2 case B``() =
    let actual = "04005AC33890" |> puzzle Case.B
    test <@ actual = 54 @>

[<Fact>]
let ``Example3 case B``() =
    let actual = "880086C3E88112" |> puzzle Case.B
    test <@ actual = 7 @>

[<Fact>]
let ``Example4 case B``() =
    let actual = "CE00C43D881120" |> puzzle Case.B
    test <@ actual = 9 @>

[<Fact>]
let ``Example5 case B``() =
    let actual = "D8005AC2A8F0" |> puzzle Case.B
    test <@ actual = 1 @>

[<Fact>]
let ``Example6 case B``() =
    let actual = "F600BC2D8F" |> puzzle Case.B
    test <@ actual = 0 @>

[<Fact>]
let ``Example7 case B``() =
    let actual = "9C005AC2F8F0" |> puzzle Case.B
    test <@ actual = 0 @>

[<Fact>]
let ``Example8 case B``() =
    let actual = "9C0141080250320F1802104A08" |> puzzle Case.B
    test <@ actual = 1 @>

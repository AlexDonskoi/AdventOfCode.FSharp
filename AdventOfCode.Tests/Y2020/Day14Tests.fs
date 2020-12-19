namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day14
open Xunit
open Swensen.Unquote

module Day14Tests =

    [<Fact>]
    let ``Verify case A example`` () =
        let input = [
            "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
            "mem[8] = 11"
            "mem[7] = 101"
            "mem[8] = 0"
        ]

        let actual = puzzle Case.A input

        test <@ actual = 165UL @>

    [<Fact>]
    let ``Verify case B mask varies`` () =
        let actual = CaseB.maskInfo "X0X1"
        test <@ actual = (4UL, [11UL; 9UL; 3UL; 1UL]) @>

    [<Fact>]
    let ``Verify case B example`` () =
        let input = [
            "mask = 000000000000000000000000000000X1001X"
            "mem[42] = 100"
            "mask = 00000000000000000000000000000000X0XX"
            "mem[26] = 1"
        ]

        let actual = puzzle Case.B input
        test <@ actual = 208UL @>
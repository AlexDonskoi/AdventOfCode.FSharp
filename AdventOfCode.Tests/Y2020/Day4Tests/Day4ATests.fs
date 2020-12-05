namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day4ATests =

    [<Fact>]
    let ``Required Birth year`` () =
        let actual = seq { "iyr:  eyr: hgt: hcl: ecl: pid: cid:" } |> Day4.run A
        test <@ actual = 0  @>

    [<Fact>]
    let ``Required Issue year`` () =
        let actual = seq { "byr:  eyr: hgt: hcl: ecl: pid: cid:" } |> Day4.run A
        test <@ actual = 0  @>

    [<Fact>]
    let ``Required Expiration year`` () =
        let actual = seq { "byr: iyr: hgt: hcl: ecl: pid: cid:"} |> Day4.run A
        test <@ actual = 0  @>

    [<Fact>]
    let ``Required Height`` () =
        let actual = seq { "byr: iyr: eyr: hcl: ecl: pid: cid:" } |> Day4.run A
        test <@ actual = 0  @>

    [<Fact>]
    let ``Required Hair Color`` () =
        let actual = seq { "byr: iyr: eyr: hgt: ecl: pid: cid:" } |> Day4.run A
        test <@ actual = 0  @>

    [<Fact>]
    let ``Required Eye Color`` () =
        let actual = seq { "byr: iyr:  eyr: hgt: hcl: pid: cid:" } |> Day4.run A
        test <@ actual = 0  @>

    [<Fact>]
    let ``Required Passport ID`` () =
        let actual = seq { "byr: iyr:  eyr: hgt: hcl: ecl: cid:" } |> Day4.run A
        test <@ actual = 0  @>

    [<Fact>]
    let ``Optional Country ID`` () =
        let actual = seq { "byr: iyr:  eyr: hgt: hcl: ecl: pid:" } |> Day4.run A
        test <@ actual = 1  @>

    [<Fact>]
    let ``Verify valid single line`` () =
        let actual = seq { "byr: iyr:  eyr: hgt: hcl: ecl: pid: cid:" } |> Day4.run A
        test <@ actual = 1  @>

    [<Fact>]
    let ``Verify valid multi line`` () =
        let actual =
           seq {
               "byr: iyr:  eyr: hgt: hcl:"
               " ecl: pid: cid:"
           }
           |> Day4.run A
        test <@ actual = 1  @>

    [<Fact>]
    let ``Verify valid multiple spaces line`` () =
        let actual = seq { "byr:   iyr:  eyr: hgt: hcl: ecl: pid: cid:" } |> Day4.run A
        test <@ actual = 1  @>

    [<Fact>]
    let ``Verify valid collection`` () =
        let actual =
            seq {
                "byr: iyr: eyr: hgt: hcl: ecl: pid: cid:"
                ""
                "byr: iyr: eyr: hgt: hcl: ecl: pid: cid:"
            }
            |> Day4.run A
        test <@ actual = 2  @>

    [<Fact>]
    let ``Verify single valid item in collection`` () =
        let actual =
            seq {
                "byr: iyr: eyr: hgt: ecl: pid: cid:"
                ""
                "byr: iyr: eyr: hgt: hcl: ecl: pid: cid:"
            }
            |> Day4.run A
        test <@ actual = 1  @>

    [<Fact>]
    let ``Verify multiline split in collection`` () =
        let actual =
            seq {
                "byr: iyr: eyr: hgt: hcl: ecl: pid: cid:"
                ""
                ""
                "byr: iyr: eyr: hgt: hcl: ecl: pid: cid:"
            }
            |>  Day4.run A
        test <@ actual = 2  @>

    let exampleInput = seq {
            "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
            "byr:1937 iyr:2017 cid:147 hgt:183cm"
            ""
            "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
            "hcl:#cfa07d byr:1929"
            ""
            "hcl:#ae17e1 iyr:2013"
            "eyr:2024"
            "ecl:brn pid:760753108 byr:1931"
            "hgt:179cm"
            ""
            "hcl:#cfa07d eyr:2025 pid:166559648"
            "iyr:2011 ecl:brn hgt:59in"
        }

    [<Fact>]
    let ``Verify example case B`` () =
        let actual = Day4.run A exampleInput
        test <@ actual = 2  @>


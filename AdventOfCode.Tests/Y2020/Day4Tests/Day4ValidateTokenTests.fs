namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day4ValidateTokenTests =

    [<Theory>]
    [<InlineData("1920")>]
    [<InlineData("1999")>]
    [<InlineData("2002")>]
    let ``Verify byr valid`` value =
        let actual =
            KeyValue("byr", value)
            |> Day4.isValidToken
        test <@ actual = true @>

    [<Theory>]
    [<InlineData("02020")>]
    [<InlineData("1919")>]
    [<InlineData("2003")>]
    let ``Verify byr invalid`` value  =
        let actual =
            KeyValue("byr", value)
            |> Day4.isValidToken
        test <@ actual = false @>

    [<Theory>]
    [<InlineData("2010")>]
    [<InlineData("2019")>]
    [<InlineData("2020")>]
    let ``Verify iyr valid`` value =
        let actual =
            KeyValue("iyr", value)
            |> Day4.isValidToken
        test <@ actual = true @>

    [<Theory>]
    [<InlineData("2009")>]
    [<InlineData("2021")>]
    let ``Verify iyr invalid`` value  =
        let actual =
            KeyValue("iyr", value)
            |> Day4.isValidToken
        test <@ actual = false @>

    [<Theory>]
    [<InlineData("2020")>]
    [<InlineData("2021")>]
    [<InlineData("2030")>]
    let ``Verify eyr valid`` value =
        let actual =
            KeyValue("eyr", value)
            |> Day4.isValidToken
        test <@ actual = true @>

    [<Theory>]
    [<InlineData("2019")>]
    [<InlineData("02020")>]
    [<InlineData("2031")>]
    let ``Verify eyr invalid`` value  =
        let actual =
            KeyValue("eyr", value)
            |> Day4.isValidToken
        test <@ actual = false @>

    [<Theory>]
    [<InlineData("150cm")>]
    [<InlineData("193cm")>]
    [<InlineData("0193cm")>]
    [<InlineData("59in")>]
    [<InlineData("76in")>]
    let ``Verify hgt valid`` value =
        let actual =
            KeyValue("hgt", value)
            |> Day4.isValidToken
        test <@ actual = true @>

    [<Theory>]
    [<InlineData("149cm")>]
    [<InlineData("194cm")>]
    [<InlineData("cm")>]
    [<InlineData("in")>]
    [<InlineData("59inin")>]
    [<InlineData("76im")>]
    let ``Verify hgt invalid`` value =
        let actual =
            KeyValue("hgt", value)
            |> Day4.isValidToken
        test <@ actual = false @>

    [<Theory>]
    [<InlineData("#000000")>]
    [<InlineData("#ffffff")>]
    [<InlineData("#123456")>]
    [<InlineData("#7890ab")>]
    [<InlineData("#cdef00")>]
    let ``Verify hcl valid`` value=
        let actual =
            KeyValue("hcl", value)
            |> Day4.isValidToken
        test <@ actual = true @>

    [<Theory>]
    [<InlineData("000000")>]
    [<InlineData("#00000")>]
    [<InlineData("#0000000")>]
    [<InlineData("#g00000")>]
    let ``Verify hcl invalid`` value =
        let actual =
            KeyValue("hcl", value)
            |> Day4.isValidToken
        test <@ actual = false @>

    [<Theory>]
    [<InlineData("amb")>]
    [<InlineData("blu")>]
    [<InlineData("brn")>]
    [<InlineData("gry")>]
    [<InlineData("grn")>]
    [<InlineData("hzl")>]
    [<InlineData("oth")>]
    let ``Verify ecl valid`` value =
        let actual =
            KeyValue("ecl", value)
            |> Day4.isValidToken
        test <@ actual = true @>

    [<Theory>]
    [<InlineData("red")>]
    [<InlineData("wat")>]
    let ``Verify ecl invalid`` value =
        let actual =
            KeyValue("ecl", value)
            |> Day4.isValidToken
        test <@ actual = false @>

    [<Theory>]
    [<InlineData("000000000")>]
    [<InlineData("012345678")>]
    [<InlineData("987654321")>]
    let ``Verify pid valid`` value =
        let actual =
            KeyValue("pid", value)
            |> Day4.isValidToken
        test <@ actual = true @>

    [<Theory>]
    [<InlineData("0000000000")>]
    [<InlineData("0123456789")>]
    [<InlineData("01234567")>]
    let ``Verify pid invalid``value =
        let actual =
            KeyValue("pid", value)
            |> Day4.isValidToken
        test <@ actual = false @>

    [<Theory>]
    [<InlineData("cid", "")>]
    [<InlineData("cid", "1")>]
    [<InlineData("any", "val")>]
    let ``Verify unchecked token`` key value =
        let actual =
            KeyValue(key, value)
            |> Day4.isValidToken
        test <@ actual = true @>

    [<Theory>]
    [<InlineData("any")>]
    [<InlineData("cid")>]
    let ``Verify unchecked value`` value =
        let actual =
            Value value
            |> Day4.isValidToken
        test <@ actual = true @>

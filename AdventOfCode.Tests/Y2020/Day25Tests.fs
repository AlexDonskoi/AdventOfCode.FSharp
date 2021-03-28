namespace AdventOfCode.Tests.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020.Day25
open Xunit
open Swensen.Unquote

module Day25Tests =

    [<Theory>]
    [<InlineData(7L, 8, 5764801L)>]
    [<InlineData(7L, 11, 17807724L)>]
    [<InlineData(17807724L, 8, 14897079L)>]
    [<InlineData(5764801L, 11, 14897079L)>]
    let ``Verify publicKey`` (subject, loopSize , expected) =
        let next = transform subject
        let actual = publicKey next subject loopSize
        test <@ actual = expected @>

    [<Theory>]
    [<InlineData(7L, 5764801L, 17807724L, 14897079L)>]
    let ``Verify encryption key`` (subject, card, door, expected) =
        let actual = encryptionKey subject (card, door)
        test <@ actual = expected @>
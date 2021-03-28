namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure

module Day25 =

    let transform multiplier = (*) multiplier >> (fun a -> a % 20201227L)

    let rec publicKey seqNext subject loopNum =
        if loopNum <= 1L then subject
            else publicKey seqNext (seqNext subject) (loopNum - 1L)

    let rec loopSearchInner seqNext target loopNum current =
        if target = current then loopNum
            else current |> seqNext |> loopSearchInner seqNext target (loopNum + 1L)

    let loopSize subject target =
        loopSearchInner (transform subject) target 1L 1L

    let encryptionKey subject (card, door) =
        let cardLoopSize = loopSize subject door
        publicKey (transform card) card (cardLoopSize - 1L)

    [<Puzzle(2020, 25)>]
    let puzzle case (input:seq<string>) =
        input
        |> Seq.toList
        |> List.map int64
        |> List.pairwise
        |> List.head
        |>
        match case with
        | Case.A -> encryptionKey 7L
        | Case.B -> encryptionKey 7L
namespace AdventOfCode.Tests.Y2020

open Xunit
open AdventOfCode.Cases.Y2020
open Swensen.Unquote

module Day5TestInput =

    type Seat =
        | X // selected
        | O // free
        | Z // not exists

    let patternData ((pattern:list<list<Seat>>), expected)=
        let rowSizes = List.map List.length pattern |> List.distinct
        let size =
            pattern |> List.length


        // verify same size
        match rowSizes with
        | [_] -> ()
        | _ -> failwith "pattern rows should be same size"

        let rowSize = List.head rowSizes
        let pair i el = i, el

        let occupiedSeats =
            [for i, row in List.mapi pair pattern do
                for j, col in List.mapi pair row do
                    match col with
                    | X -> yield i * rowSize + j
                    | _ -> ()]

        (List.length pattern, rowSize, occupiedSeats, expected)



    let availableSeatsInput() =
        seq {
            [[O]], [] // pilot place

            [[O]
             [O]], []

            [[X; O; X]
             [X; X; X]], [] // very front row

            [[X; X; X]
             [X; O; X]], [] // very back row

            [[X; O; X]
             [X; O; X]
             [X; O; X]], [4] // only in center

            [[X; O; X]
             [O; O; X]
             [X; O; X]], [] // not occupied at left

            [[X; X; X]
             [O; X; X]
             [X; O; X]], [3] // fine

            [[X; X; X]
             [X; X; O]
             [X; O; X]], [5] // fine

            [[X; X; X]
             [X; X; O]
             [Z; O; X]], [] // no place at back row

            [[X; X; Z]
             [O; X; X]
             [X; O; X]], [] // no place at front row

            [[Z; Z; X]
             [O; X; X]
             [X; O; X]], [3] // missed two places at from but -1 is available

            [[O; X; X]
             [X; X; O]
             [O; X; X]
             [O; O; O]], [] // empty +1 seat

            [[O; X; X]
             [O; X; O]
             [X; O; X]
             [Z; X; X]], [3;5;7] // all in middle

            [[O; X; X]
             [X; X; X]
             [X; O; X]
             [Z; Z; Z]], [] // ignore lat row as no seats available
        }
        |> Seq.map patternData

module Day5Tests =

    [<Theory>]
    [<InlineData("BFFFBBFRRR", 567)>]
    [<InlineData("FFFBBBFRRR", 119)>]
    [<InlineData("BBFFBBFRLL", 820)>]
    [<InlineData("BL", 2)>]
    [<InlineData("FR", 1)>]
    [<InlineData("FL", 0)>]
    [<InlineData("F", 0)>]
    [<InlineData("B", 1)>]
    let ``Parse boarding pass`` input seat =
        let actual = Day5.seatId input
        test <@ actual = seat  @>

    type Data() as this =
        inherit TheoryData<int, int, list<int>, list<int>>()
        do [for x in Day5TestInput.availableSeatsInput() do this.Add x] |> ignore



    [<Theory>]
    [<ClassData(typeof<Data>)>]
    let ``Verify boarding pattern`` rows cols occupied expected =
        let actual = Day5.availableSeats (rows, cols) occupied
        test <@ actual = expected  @>
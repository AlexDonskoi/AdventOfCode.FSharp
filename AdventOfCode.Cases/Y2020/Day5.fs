namespace AdventOfCode.Cases.Y2020

open System
open AdventOfCode.Cases.Infrastructure

module Day5 =

    let seatId: seq<char> -> int =
        let folder acc = function
            | 'B' -> acc * 2 + 1
            | 'F' -> acc * 2
            | 'L' -> acc * 2
            | 'R' -> acc * 2 + 1
            | _ -> failwith "unsupported directive"
        Seq.fold folder 0

    let stats (rows, cols)=
        let folder (min, max, free) seat =
            let currentRow:int = seat / cols
            Math.Min(min, currentRow), Math.Max(max, currentRow), List.except [seat] free

        // generate all possible seatId
        let allSeats = [ for x in [0 .. rows*cols-1] -> x ]

        // get min, max occupied rows num and free seats
        Seq.fold folder (rows-1, 0, allSeats)

    let availableSeats (rows, cols) src =

        let (minRow, maxRow, free) = stats (rows, cols) src
        let minSeat, maxSeat = (minRow + 1) * cols, maxRow * cols - 1

        // filter seats with occupied +1 and -1
        [ for x in free do
            match List.except free [x - 1; x + 1] with
            | [_;_] -> yield x
            | _ -> ()]
       // exclude very front and back
       |> List.filter (fun el -> el >= minSeat && el <= maxSeat)

    [<Puzzle(2020, 5)>]
    let puzzle case (source:seq<string>) =
        source
        |> Seq.map seatId
        |> match case with
            | Case.A -> Seq.max
            | Case.B -> availableSeats (127, 7) >> List.head



namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser

module Day13 =

    let rec gcf a b =
        if b = 0L then a
            else gcf b (a % b)

    let rec lcm a b = (a / (gcf a b)) * b

    let parseSchedule input =
        [for i,x in  String.split "," input |> Array.mapi (fun ind el -> ind, el) do
             match x with
             | Int64 n -> yield n, int64 i
             | _ -> ()]

    let parseInput = function
        | [Int64 start; schedule] ->
            (start, parseSchedule schedule)
        | _ -> failwith "unexpected input format"

    let withWaitTime start (schedule,_) =
        (schedule, schedule - (start % schedule))

    let caseA start schedules =
        let (busNo, waitTime) =
            List.map (withWaitTime start) schedules
            |> List.minBy snd
        busNo * waitTime


    let rec nextMatchTimestamp (accTimestamp, accFreq) (stepTimestamp, stepFreq, skip) =
        match compare accTimestamp stepTimestamp with
        | 0 -> accTimestamp
        | -1 -> nextMatchTimestamp (accTimestamp + accFreq, accFreq) (stepTimestamp + skip, stepFreq, skip)
        | _ -> nextMatchTimestamp (accTimestamp, accFreq) (stepTimestamp + stepFreq, stepFreq, skip)

    let matchNextSchedule (accTimestamp, accFreq) (stepFreq, stepShift) =
        let rem = accTimestamp % stepFreq
        if rem = stepShift then (accTimestamp, lcm accFreq stepFreq)
            else
                // closes timestamp to accTimestamp when step bus arrives
                let stepTimestamp = accTimestamp + stepFreq - rem
                // accFreq grows fast ()
                let skipValue = (accFreq / stepFreq) * stepFreq
                let nextMatch = nextMatchTimestamp (accTimestamp, accFreq) (stepTimestamp - stepShift, stepFreq, skipValue)
                (nextMatch, lcm accFreq stepFreq)

    let caseB _ schedule:int64 =
        match schedule with
        | (h, _)::rest ->
            List.fold matchNextSchedule (0L, h) rest
            |> fst
        | _ -> failwith "empty array"

    [<Puzzle(2020, 13)>]
    let puzzle case (input:seq<string>) =
        input
        |> Seq.toList
        |> parseInput
        ||>
        match case with
        | Case.A -> caseA
        | Case.B -> caseB
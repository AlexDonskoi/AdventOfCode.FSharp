namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser

module Day15 =

    module CaseA =

        let init =
             let folder (turn, lastValue, map) cur =
                (turn + 1, cur, Map.add lastValue turn map)
             Array.fold folder (0, -1, Map.empty)

        let rec turnValue stopTurn = function
            | (turn, lastValue, _) when turn >= stopTurn -> lastValue
            | (turn, lastValue, map) ->
                let nextNum =
                    match Map.tryFind lastValue map with
                    | Some lastSeen -> turn - lastSeen
                    | None -> 0
                turnValue stopTurn (turn + 1, nextNum, Map.add lastValue turn map)
        let run stopTurn = init >> turnValue stopTurn

    [<Puzzle(2020, 15)>]
    let puzzle case (input:string) =
        input
        |> String.split ","
        |> Array.map int
        |>
        match case with
        | Case.A -> CaseA.run 2020
        | Case.B -> CaseA.run 30000000


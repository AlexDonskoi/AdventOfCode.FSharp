module AdventOfCode.Cases.Y2021.Day7

open AdventOfCode.Cases.Infrastructure

let costB distance = (distance * distance + distance) /2L
let costA distance = distance
let costAll cost pos = List.fold (fun acc cur -> pos - cur |> abs |> cost |> (+) acc) 0L

let rec next cost compare src =
    let decrement = (+) -1L
    let state = List.map decrement src
    let stateCost = List.sumBy (abs >> cost) state
    if compare > 0L && stateCost > compare then compare
        else next cost stateCost state

[<Puzzle(2021, 7)>]
let puzzle case (input:string) =
    input
    |> String.split ","
    |> Array.map int64
    |> Array.toList
    |> match case with
        | Case.A -> next costA 0L
        | Case.B -> next costB 0L

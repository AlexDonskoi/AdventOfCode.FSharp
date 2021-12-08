module AdventOfCode.Cases.Y2021.Day8

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections

let parse =
    let split = String.split " "
    String.split "|" >>
    function
    | [| patterns; output |] -> patterns, split output
    | _ -> failwith "incorrect input"

let byWeight = function
    | 42 -> 0
    | 17 -> 1
    | 34 -> 2
    | 39 -> 3
    | 30 -> 4
    | 37 -> 5
    | 41 -> 6
    | 25 -> 7
    | 49 -> 8
    | 45 -> 9
    | _ -> failwith "Unknown segment"

let weights = Seq.countBy id >> Map

let number weights  = Seq.sumBy (Map.find >> (|>) weights) >> byWeight

let caseA = Seq.sumBy <| function |1 |4 |7 |8 -> 1 | _ -> 0

let caseB = (*) 10 >> (+) |> Seq.fold <| 0

[<Puzzle(2021, 8)>]
let puzzle case (input:seq<string>) =
    let numbers (patterns, outputs) = outputs |> Seq.map (weights patterns |> number)
    let result =
        match case with
        | Case.A -> caseA
        | Case.B -> caseB
    input
    |> Seq.sumBy (parse >> numbers >> result)

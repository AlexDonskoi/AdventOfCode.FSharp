module AdventOfCode.Cases.Y2015.Day8

open System
open AdventOfCode.Cases.Utilities
open AdventOfCode.Cases.Infrastructure.Parser
open AdventOfCode.Cases.Infrastructure

let rec diffA acc =
    function
    | [] -> acc + 2
    | '\\'::'x'::rest ->
        let rest = List.skip 2 rest
        diffA <| acc + 3 <| rest
    | '\\'::_::rest -> diffA <| acc + 1 <| rest
    | _::rest -> diffA acc rest

let rec diffB acc =
    function
    | [] -> acc + 4
    | '\\'::'x'::rest ->
        let rest = List.skip 2 rest
        diffB <| acc + 1 <| rest
    | '\\'::_::rest -> diffB <| acc + 2 <| rest
    | _::rest -> diffB acc rest

[<Puzzle(2015, 8)>]
let puzzle case (input:seq<string>) =
    match case with
        | A -> diffA
        | B -> diffB
    <| 0
    |> (>>) Seq.toList
    |> Seq.sumBy
    <| input
module AdventOfCode.Cases.Y2024.Day7
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let parseItems = String.split " " >> Seq.map int64 >> Seq.toList

let parseLine = String.split ": " >> function
    | [| Int64 tgt; items |] ->
        tgt, parseItems items
    | _ -> failwith "incorrect line"
    
let rec checkA tgt = function
    | [] -> Seq.length tgt > 0
    | [v] -> List.contains v tgt
    | h::rest ->
        checkA
        <| [
            for v in tgt do
                if v % h = 0L then yield v/h else ()
                if v>=h then yield v-h else ()
        ]
        <| rest
    
let mapA tgt items = if checkA [tgt] items then tgt else 0L

let resultA v = v ||> mapA

let resultB v = 0

[<Puzzle(2024, 7)>]
let puzzle case (source:seq<string>) =
    let res =
        match case with
        | Case.A -> resultA
        | Case.B -> resultA
    source
    |> Seq.map parseLine
    |> Seq.sumBy res







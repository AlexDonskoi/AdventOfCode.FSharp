module AdventOfCode.Cases.Y2023.Day15
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let parseInput = String.replace Environment.NewLine "" >> String.split "," >> Array.toList >> List.map Seq.toList

let rec hash acc = function
    | [] -> acc
    | h::rest ->
        let value =
            int h |> (+) acc
            |> (*) 17
            |> (%) <| 256
        hash value rest


[<Puzzle(2023, 15)>]
let puzzle case (source:string) =
    let source = source |> parseInput
    match case with
    | Case.A -> List.fold (fun acc cur -> hash 0 cur |> (+) acc) 0 source
    | Case.B -> 0
        
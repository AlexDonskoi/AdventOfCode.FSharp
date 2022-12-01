module AdventOfCode.Cases.Y2022.Day1
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let folder acc cur = 
    let (list, tmp) = acc
    match cur with
    | Int x -> list, (tmp + x)        
    | _ -> tmp::list, 0
        

[<Puzzle(2022, 1)>]
let puzzle case (source:seq<string>) =
    let takeCount =
        match case with
        | Case.A -> 1
        | Case.B -> 3
    source
    |> Seq.fold folder ([], 0)
    |> fst
    |> List.sortDescending
    |> List.take takeCount
    |> List.sum







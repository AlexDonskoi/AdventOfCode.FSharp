module AdventOfCode.Cases.Y2023.Day9
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

  
let parseRow = String.split " " >> Seq.toList >> List.map int64

let rec getSeqA acc src =
    let tgt = List.last src
    let acc = tgt::acc
    if src |> List.forall ((=) tgt) then acc
        else
            let src = src |> List.pairwise |> List.map (fun (a,b) -> b - a)
            getSeqA acc src


let rec getSeqB acc src =
    let tgt = List.head src
    let acc = tgt::acc
    if src |> List.forall ((=) tgt) then acc
        else
            let src = src |> List.pairwise |> List.map (fun (a,b) -> b - a)
            getSeqB acc src

[<Puzzle(2023, 9)>]
let puzzle case (source:seq<string>) =
    source
    |> match case with
        | Case.A ->  Seq.sumBy (parseRow >> getSeqA [] >> Seq.sum )
        | Case.B ->
            let folder acc cur = cur - acc
            Seq.sumBy (parseRow >> getSeqB [] >> Seq.fold folder 0L)
    
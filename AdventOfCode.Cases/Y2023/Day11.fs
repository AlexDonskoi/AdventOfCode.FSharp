module AdventOfCode.Cases.Y2023.Day11
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let collectRow ind row =
    row
    |> Seq.indexed
    |> Seq.filter (snd >> (=) '#')
    |> Seq.map (fun (i,v) -> ind,i)
  
let collect =    
    Seq.indexed
    >> Seq.collect ( (<||)collectRow)
    >> Set.ofSeq
    
[<Puzzle(2023, 11)>]
let puzzle case (source:seq<string>) =
    let source = collect source
    let rows = source |> Seq.map fst |> Set.ofSeq
    let columns = source |> Seq.map snd |> Set.ofSeq
    let replace =
        match case with
        | Case.A -> 2L
        | Case.B -> 1000000L
        
    let diff set s f =
        let mx = max f s
        let mn  = min f s
        let len = set |> Seq.filter (fun i -> i > mn && i <= mx) |> Seq.length
        int64 (mx - mn) + (replace - 1L)* int64 (mx - mn  - len) 
    let path columns rows (si, sj) (fi, fj) =
        diff rows si fi
        |> (+) <| diff columns sj fj
    let path = path columns rows
        
    Seq.allPairs source source
            |> Seq.sumBy ((<||) path)
            |> (/) <| 2L
            
            
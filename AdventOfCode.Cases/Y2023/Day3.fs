module AdventOfCode.Cases.Y2023.Day3
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let parseInput src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 '.'
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v]] |> ignore
    map
    
let check map (ir, jc) len =
    let size1, size2 = Array2D.sizes map
    [for i in ir-1..ir+1 do
         for j in jc-1..jc+len do
             let res = j < 0 || i < 0 || i > size1 || j > size2 || map.[i,j] = '.' || (map.[i,j] >= '0' && map.[i,j] <= '9')
             yield res |> not]
    |> Seq.fold (||) false            
    
let folderItem check =
    let rowSelector (ind, src) =
        let selector (m:Match) =
            match m.Value with
            | Int v ->
                if check (ind, m.Index) (String.length m.Value) then v else 0
            | v -> failwith $"cannot parse ${v}"
                
        Regex.Matches(src, "\d+")
        |> Seq.sumBy selector
        
    Seq.indexed >> Seq.sumBy rowSelector
    
let gears map (ir, jc) len =
    let size1, size2 = Array2D.sizes map
    [for i in ir-1..ir+1 do
         for j in jc-1..jc+len do
             if j > 0 && i > 0 && i <= size1 && j <= size2 && map.[i,j] = '*'
             then yield (i, j)
                 else ()]             
    
let folderItemB gears: seq<string>->int =
    let rowSelector (ind, src) =
        [ for m in Regex.Matches(src, "\d+") do
            match m.Value with
            | Int v ->
                for g in gears (ind, m.Index) (String.length m.Value) do
                    g, v
            | v -> failwith $"cannot parse ${v}"]
    let folder acc (g, v) =
        Map.change g (function | None -> Some [v] | Some arr -> Some (v::arr)) acc
        
    Seq.indexed
    >> Seq.collect rowSelector
    >> Seq.fold folder Map.empty
    >> Map.filter (fun k v -> List.length v = 2)
    >> Map.values
    >> Seq.map (Seq.fold (*) 1)
    >> Seq.sum
        

[<Puzzle(2023, 3)>]
let puzzle case (source:seq<string>) =
    
    match case with
    | Case.A ->
        let checkSrc = source |> parseInput |> check
        folderItem checkSrc source
    | Case.B ->
        let checkSrc = source |> parseInput |> gears
        folderItemB checkSrc source





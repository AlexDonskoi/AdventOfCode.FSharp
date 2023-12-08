module AdventOfCode.Cases.Y2023.Day8
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

  
let parseRow input =
    let captures = Regex.Match(input, $"(?<src>\w+) = \((?<left>\w+), (?<right>\w+)\)") |> Regex.captures >> List.head
    let src = captures "src"
    let left = captures "left"
    let right = captures "right"
    src, (left, right)
    
    
let rec cycleMove map target acc step moves pos =
    match moves with
    | [] -> pos, acc
    | mv::rest ->
        let choose = if mv = 'L' then fst else snd         
        let newPos = map |> Map.find pos |> choose
        let acc = if target newPos then Set.add step acc else acc
        cycleMove map target acc  <| step + 1L <| rest <| newPos
        
let rec speedup maps size step =
    if step <= 0 then size, maps else
        let update k (e,v) =
            let pos, ext = Map.find e maps
            let set = ext |> Set.map ((+) size) |> Set.union v
            pos, set
        maps
        |> Map.map update
        |> speedup
        <| size * 2L
        <| step - 1
        
let rec findExit maps pos step =
    let cycleMove = List.map (fun k -> Map.find k maps) pos
    let res = cycleMove |> List.map snd |> Set.intersectMany
    if Set.isEmpty res then findExit maps <| List.map fst cycleMove <| step + 1L
        else
            step, Set.minElement res
    
[<Puzzle(2023, 8)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.toList
    let movesMap = source |> List.head |> Seq.toList
    let maps = source |> List.skip 1 |> List.map parseRow |> Map.ofList
    let start, exit = 
        match case with
        | Case.A -> ["AAA"], ((=) "ZZZ")                      
        | Case.B ->
            (Map.keys maps |> Seq.filter (Seq.last >> (=) 'A') |> Seq.toList), (Seq.last >> (=) 'Z')
            
    let cycleMove = cycleMove maps exit Set.empty 1 movesMap
    let maps = maps |> Map.map (fun k _ -> cycleMove k)
    let size = List.length movesMap |> int64
    let size, maps = speedup maps size 20
    
    let ccl,st = findExit maps start 0L
    ccl * size + st
    
    
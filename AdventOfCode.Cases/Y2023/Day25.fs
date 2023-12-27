module AdventOfCode.Cases.Y2023.Day25
open System
open System.Diagnostics
open System.Globalization
open System.Net.Sockets
open System.Numerics
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Threading.Tasks
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let parseLine = String.split ": " >> function
    | [| src; targets |] -> src, String.split " " targets
    | _ -> failwith "wtf"

let appendOption v = Option.defaultValue Set.empty >> Set.add v >> Some
    
let insertMap map k v =
    map
    |> Map.change k (appendOption v)
    |> Map.change v (appendOption k)
  
let rec getPathRec map skip current exit =
    let tmp = Set.minElement current
    let steps, point, visited = tmp
    let current = Set.remove tmp current
    if point = exit then true,visited else
        let visited = Set.add point visited
        let current =
            Map.find point map
            |> Set.fold (fun acc c -> if Set.contains (point, c) skip || Set.contains c visited then acc else Set.add (steps + 1, c, visited) acc) current
        if Set.isEmpty current then false, visited else
            getPathRec map skip current exit
            
let getPath map skip a b = getPathRec map skip <| Set.singleton (0, a, Set.singleton a) <| b
  
[<Puzzle(2023, 25)>]
let puzzle case (source:seq<string>) =
    let folder map (s, arr) =
        Seq.fold (fun acc -> insertMap acc s) map arr
    let source = source |> Seq.map parseLine |> Seq.fold folder Map.empty
    
    let allPairs = Map.fold (fun acc k lst -> lst |> Seq.map (fun c -> min k c, max c k) |> Set.ofSeq |> Set.union acc) Set.empty source  |> Set.toArray |> Array.rev
    
    let size = Array.length allPairs |> (+) -1
    
    let mutable res = 0
    let mutable cache = Set.empty;
    for startPair in source do
        if res > 0 then () else 
            let start1 = startPair.Key
            for end1 in startPair.Value do
                if res > 0 then () else
                    let skip = Set.empty |> Set.add (min start1 end1, max start1 end1)  |> Set.add (max start1 end1, min start1 end1)
                    let st = min start1 end1
                    let fn = max start1 end1
                    let skip = skip |> Set.add (st, fn)  |> Set.add (fn,st)
                    if Set.contains (st, fn) cache then () else                                            
                        match getPath source skip start1 end1 with
                        | false, _ -> failwith "too earlier"
                        | _, visited2 ->
                            for start2 in visited2 do
                                if res > 0 then () else
                                    for end2 in Map.find start2 source do
                                        if res > 0 || (Set.contains end2 visited2 |> not) then () else
                                            let st = min start2 end2
                                            let fn = max start2 end2
                                            let skip = skip |> Set.add (st, fn)  |> Set.add (fn,st)
                                            if Set.count skip <> 4 || Set.contains (st, fn) cache then () else
                                                match getPath source skip start1 end1 with
                                                | false, _ -> failwith "too earlier"
                                                | _, visited3 ->
                                                    for start3 in visited3 do
                                                        if res > 0 then () else
                                                            for end3 in Map.find start3 source do
                                                            if res > 0  || (Set.contains end3 visited3 |> not) then () else
                                                                let st = min start3 end3
                                                                let fn = max start3 end3
                                                                let skip = skip |> Set.add (st, fn)  |> Set.add (fn,st)
                                                                if Set.count skip <> 6 || Set.contains (st, fn) cache then () else
                                                                    match getPath source skip start1 end1 with
                                                                    | true, _ -> ()
                                                                    | _, visited -> 
                                                                        let grpCount = Set.count visited
                                                                        let allCount = Map.count source
                                                                        res <-(grpCount)* (allCount - grpCount)
                                                                          
                        cache<- Set.add (st, fn) cache                                                    
    match case with
    | Case.A -> res
    | Case.B -> failwith "all done!!"
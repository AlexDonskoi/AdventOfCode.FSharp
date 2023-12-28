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
open AdventOfCode.Cases.Y2021.Day13
open Microsoft.FSharp.Core

let parseLine = String.split ": " >> function
    | [| src; targets |] -> src, String.split " " targets
    | _ -> failwith "wtf"

let appendOption v = Option.defaultValue Set.empty >> Set.add v >> Some
    
let insertMap map k v =
    map
    |> Map.change k (appendOption v)
    |> Map.change v (appendOption k)
  
let rec getPathRec map skip current visited exit =
    if Set.isEmpty current then false, visited else
        let tmp = Set.minElement current
        let steps, point = tmp
        let current = Set.remove tmp current
        if point = exit then true, Map.add point steps visited
        else if Map.containsKey point visited then getPathRec map skip current visited exit
        else
            let visited = Map.add point steps visited
            let current =
                Map.find point map
                |> Set.fold (fun acc c -> if Set.contains (point, c) skip then acc else Set.add (steps + 1, c) acc) current
            
            getPathRec map skip current visited exit
  
let rec reversePath connections skip points path tgt exit =
    if tgt = exit then path else
        let steps = Map.find tgt points |> (+) -1
        let tgt = Map.find tgt connections |> Set.fold (fun acc c -> if Set.contains c path || (Map.tryFind c points <>Some steps ) || (Set.contains (tgt,c) skip) then acc else c) ""
        let path = Set.add tgt path
        reversePath connections skip points path tgt exit
        
            
let getPath map skip a b =
     match getPathRec map skip <| Set.singleton (0, a)<| Map.empty  <| b with
     | false, visited -> false, visited |> Map.keys |> Set.ofSeq
     | _, points ->
         true,reversePath map skip points Set.empty b a
        
  
let getResult map group =   
    let grpCount = Set.count group
    let allCount = Map.count map
    (grpCount)* (allCount - grpCount)
  
[<Puzzle(2023, 25)>]
let puzzle case (source:seq<string>) =
    let folder map (s, arr) =
        Seq.fold (fun acc -> insertMap acc s) map arr
    let source = source |> Seq.map parseLine |> Seq.fold folder Map.empty
    
    let allPairs = Map.fold (fun acc k lst -> lst |> Seq.map (fun c -> min k c, max c k) |> Set.ofSeq |> Set.union acc) Set.empty source  |> Set.toArray |> Array.rev
    
    let size = Array.length allPairs |> (+) -1
    
    let mutable res = 0
    let mutable cache = Set.empty
    
    for startPair in source do
        if res > 0 then () else 
            let start1 = startPair.Key
            for end1 in startPair.Value do
                if res > 0 then () else
                    let st = min start1 end1
                    let fn = max start1 end1
                    let skip = Set.empty |> Set.add (st, fn)  |> Set.add (fn,st)
                    match getPath source skip start1 end1 with
                    | false, _ -> failwith "too earlier"
                    | _, visited2 ->
                        for start2 in visited2 do
                            if res > 0 then () else
                                let ends2 = Map.find start2 source
                                let skip2 = ends2 |> Set.fold (fun acc c -> acc |> Set.add (start2, c) |> Set.add (c, start2)) skip
                                match getPath source skip2 start1 end1 with
                                | false, _ ->
                                    for end2 in ends2 do
                                        let skip2 = skip |> Set.add (start2, end2) |> Set.add (end2, start2)
                                        if Set.count skip2 <> 4 || Set.contains (start2, end2) cache then () else
                                            match getPath source skip2 start1 end1 with
                                            | false, _ -> failwith "too earlier"
                                            | _, visited3 ->
                                                for start3 in visited3 do
                                                if res > 0 then () else
                                                    let ends3 = Map.find start3 source
                                                    let skip3 = ends3 |> Set.fold (fun acc c -> acc |> Set.add (start3, c) |> Set.add (c, start3)) skip2
                                                    match getPath source skip3 start1 end1 with
                                                    | true, _ -> ()
                                                    | _ ->
                                                        for end2 in ends2 do
                                                            let skip2 = skip |> Set.add (start2, end2) |> Set.add (end2, start2)
                                                            for end3 in ends3 do
                                                                let skip3 = skip2 |> Set.add (start3, end3) |> Set.add (end3, start3)
                                                                if Set.count skip3 <> 6  || Set.contains (start2, end2) cache  || Set.contains (start3, end3) cache then () else
                                                                    match getPath source skip3 start1 end1 with
                                                                    | true, _ -> ()
                                                                    | _, visited -> res <-getResult source visited
                                                    
                                                
                                | _, visited3 ->
                                    for start3 in visited3 do
                                        if res > 0 then () else
                                            let ends3 = Map.find start3 source
                                            let skip3 = ends3 |> Set.fold (fun acc c -> acc |> Set.add (start3, c) |> Set.add (c, start3)) skip2
                                            match getPath source skip3 start1 end1 with
                                            | true, _ -> ()
                                            | _ ->
                                                for end2 in ends2 do
                                                    let skip2 = skip |> Set.add (start2, end2) |> Set.add (end2, start2)
                                                    for end3 in ends3 do
                                                        let skip3 = skip2 |> Set.add (start3, end3) |> Set.add (end3, start3)
                                                        if Set.count skip3 <> 6  || Set.contains (start2, end2) cache  || Set.contains (start3, end3) cache then () else
                                                            match getPath source skip3 start1 end1 with
                                                            | true, _ -> ()
                                                            | _, visited -> res <-getResult source visited
                    cache<- cache |> Set.add (start1, end1) |> Set.add (end1, start1)                                            
                                                                                                          
    match case with
    | Case.A -> res
    | Case.B -> failwith "all done!!"
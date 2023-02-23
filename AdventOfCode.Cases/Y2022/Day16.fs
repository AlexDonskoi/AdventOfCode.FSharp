module AdventOfCode.Cases.Y2022.Day16
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let sourceRegex = Regex "Valve (?<src>\w+) has flow rate=(?<rate>\d+); tunnels? leads? to valves?( (?<path>\w+),?)+"

type Pipe = string*int*list<string>

let parse input =
    let matches = sourceRegex.Match input
    let src =
        match Regex.captures matches "src" with
        | [v] -> v
        | _ -> failwith "incorrect source"
    let rate =
        match Regex.captures matches "rate" with
        | [Int v] -> v
        | _ -> failwith "incorrect source"
    let path =
        match Regex.captures matches "path" with
        | [] -> failwith "incorrect source"
        | v -> v
    src, rate, path

let rec moveWeightRec connections src tgt visited iter =
    if Set.contains tgt src then iter
    else
        let collector src = connections |> Map.find src |> snd
        let moveOptions = Seq.collect collector src |> Seq.except visited |> Set.ofSeq
        moveWeightRec connections moveOptions tgt visited (iter + 1)

let rec moveWeight connections src tgt = moveWeightRec connections (Set.singleton src) tgt Set.empty 0

let moveOptions connections iter state =
    let position, valves = state
    let rate, moves = Map.find position connections
    [
        yield! [for m in moves -> m, valves] // move
        if rate > 0 && (Map.containsKey position valves |> not) then // open
            let valves = Map.add position (rate * iter) valves
            yield position, valves
    ]

let rec move connections states = function
    | 0 -> states |> List.map snd
    | iter ->
        let collector = moveOptions connections iter
        let states = List.collect collector states
        move connections states (iter - 1)

let searchOption maxTime valves weights current =
    let released, visited, spent, valve = current
    [
     for (move, rate) in valves do

         if Set.contains move visited then ()
         else
             let moveWeight = Map.find (valve,move) weights
             if (moveWeight + spent >= maxTime) then()
             else
                 let visited = Set.add move visited
                 let spent = spent + moveWeight + 1
                 let released = released + rate * (maxTime - spent)
                 released, visited, spent, move
         ]

let rec search searchOption state =
    [for opt in searchOption state do
            yield opt
            yield! search searchOption opt
   ]



[<Puzzle(2022, 16)>]
let puzzle case (source:seq<string>) =
    let connections =
        source
        |> Seq.map (parse >> fun (src,rate,path) -> src, (rate,path))
        |> Map.ofSeq

    let valves = Map.map (fun _ v -> fst v) connections |> Map.filter (fun _ v -> v > 0) |> Map.toList
    let moveWeight = moveWeight connections
    let weights = "AA"::(List.map fst valves)
    let weights = List.allPairs weights weights
                  |> List.collect (fun (f,s) -> [f,s; f,s])
                  |> Seq.map (fun (f,s) -> (f,s), moveWeight f s)
                  |> Map.ofSeq


    match case with
    | Case.A ->
        let searchOption = searchOption 30 valves weights
        search searchOption (0, Set.singleton "AA", 0, "AA")
        |> List.map (fun (v,_,_,_) -> v) |> List.max


    | Case.B ->
        let searchOption = searchOption 26 valves weights
        let items =
            search searchOption (0, Set.singleton "AA", 0, "AA")
            |> List.sortByDescending (fun (ra, va, _, _) -> ra)
        let rec findRecursive = function
            | [_] |[] -> failwith "wtf"
            | (ra,va, _, _)::rest ->
                match List.tryFind (fun (_,vb, _, _) -> Set.intersect va vb |> Set.count <= 1) rest with
                | None -> findRecursive rest
                | Some (rb,_, _, _) -> ra + rb

        findRecursive items


module AdventOfCode.Cases.Y2022.Day16
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

[<Puzzle(2022, 16)>]
let puzzle case (source:seq<string>) =
    let connections =
        source
        |> Seq.map (parse >> fun (src,rate,path) -> src, (rate,path))
        |> Map.ofSeq

    match case with
    | Case.A ->
        let valves = Map.filter (fun _ v -> fst v |> (<) 0) connections |> Map.keys |> List.ofSeq
        let moveWeight = moveWeight connections
        let weights = "AA"::valves
        let weights = List.allPairs weights weights
                      |> List.collect (fun (f,s) -> [f,s; f,s])
                      |> Seq.map (fun (f,s) -> (f,s), moveWeight f s)
                      |> Map.ofSeq
        let rec paths valves weights acc =
            let maxLen = List.map (snd >> List.length) acc
            let maxLen = if List.isEmpty acc then 0 else List.max maxLen
            if Seq.length valves |> (=) maxLen then acc
            else
            let acc = [
                for w, lst in acc do
                    //if w
                    let add = Seq.except lst valves
                    let head = Seq.tryHead lst |> Option.defaultValue "AA"
                    yield! [
                        for a in add do
                            let w = Map.find (head, a) weights |> (+) w
                            if w < 31 then w, a::lst
                    ]

            ]
            paths valves weights acc
        let moveOptions = paths valves weights <| List.map (fun v -> Map.find ("AA",v) weights, [v]) valves
        0
    | Case.B -> 0


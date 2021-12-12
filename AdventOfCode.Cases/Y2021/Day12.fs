module AdventOfCode.Cases.Y2021.Day12

open System.Collections.Generic
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let parseLine = String.split "-" >> function
    | [| f; s |] -> f,s
    | _ -> failwith "incorrect path"

let addRoute start finish =
    match start, finish with
    | _, "start" -> id
    | "end", _ -> id
    | s,f ->
        let update = Option.defaultValue [] >> List.append [finish] >> Some
        Map.change start update

let add (f,s) = addRoute f s >> addRoute s f

let map = Seq.fold (fun acc cur -> add cur acc) Map.empty

let finish = "end"
let start = "start"

let next connections (duplicates, path) =
    [for c in connections path ->
        let isSmall = c <> String.upper c
        let duplicates = duplicates + (if isSmall && List.contains c path then 1 else 0)
        duplicates, c::path ]
    |> function
        | [] -> [(duplicates, path)]
        | v -> v

let rec allPaths filter connections paths completed =
    match paths with
    | [] -> completed
    | paths ->
        let newCompleted, rest =
            [for flag, path in paths do yield! next connections (flag, path) |> List.filter (fst >> filter)]
            |> List.partition (snd >> List.head >> (=) finish)
        let completed = List.length newCompleted + completed
        allPaths filter connections rest completed

let filterA  = (=) 0
let filterB  = (>) 2


[<Puzzle(2021, 12)>]
let puzzle case (input:seq<string>) =
    let map = input |> Seq.map parseLine |> map
    let start = List.singleton (0, [start])
    let connections path = List.head path |> Map.tryFind <| map |> Option.defaultValue []
    allPaths
    <| match case with
        | Case.A -> filterA
        | Case.B -> filterB
    <| connections
    <| start <| 0

module AdventOfCode.Cases.Y2021.Day14

open System
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

open AdventOfCode.Cases.Infrastructure.Parser
let parseDot = String.split "," >> function
    | [| Int f; Int s |] -> f,s
    | _ -> failwith "incorrect dot"

let parseInsertion = String.split " -> " >> function
    | [| pair ; add |] ->
        match Seq.toList pair, Seq.toList add with
        | [f;s], [add] -> (f,s), add
        | _ -> failwith "incorrect line"
    | _ -> failwith "incorrect line"

let parse = String.split $"{Environment.NewLine}{Environment.NewLine}" >> function
    | [| template; insertions |] ->
        let insertions =
            insertions
            |> String.split $"{Environment.NewLine}"
            |> Seq.map parseInsertion
            |> Map
        Seq.toList template, insertions
    | _ -> failwith "incorrect input"

let increase key value = Map.change key (Option.defaultValue 0L >> (+) value >> Some)

let initStats template =
    List.pairwise template
    |> List.fold (fun acc p -> increase p 1L acc) Map[]

let rec iter (insertions:Map<char*char, char>) count template =
    match count with
    | 0 -> template
    | _ ->
        let folder map state (f,s) cnt =
            state
            |>  match Map.tryFind (f,s) map with
                | Some add -> increase (f,add) cnt >> increase (add,s) cnt
                | _ -> increase (f,s) cnt
        Map.fold <| folder insertions <| Map [] <| template
        |> iter insertions (count - 1)

let result stats =
    stats
    |> Map.fold (fun acc (f,s) cnt -> increase f cnt acc |> increase s cnt) (Map [])
    |> Map.map (fun _ v -> (v + 1L) >>> 1 )// each item count twice except first and last.
    |> Map.fold (fun (mx, mn) _ cnt -> Option.max cnt mx, Option.min cnt mn) (None, None)
    |> function
        | Some mx, Some mn -> mx - mn
        | _, _ -> failwith "no max or min found"

let run input count =
    let template, insertions = parse input
    initStats template
    |> iter insertions count
    |> result

[<Puzzle(2021, 14)>]
let puzzle case (input:string) =
    run input
    <|match case with
        | Case.A -> 10
        | Case.B -> 40

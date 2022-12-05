module AdventOfCode.Cases.Y2022.Day5
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let parseMove src =
    match String.split "move " src with
    | [| src |] ->
        match String.split " from " src with
        | [| Int quantity; src |] ->
            match String.split " to " src with
            | [| Int from; Int toInd |] -> (quantity, from - 1,toInd - 1)
            | _ -> failwith "incorrect format"
        | _ -> failwith "incorrect format"
    | _ -> failwith "incorrect format"

let extract ind quantity state =
    let target = List.skip ind state  |> List.head
    let crate = List.take quantity target
    [for i,v in List.indexed state ->
        if ind = i then List.skip quantity v
            else v ], crate

let put values ind state =
    [for i,v in List.indexed state ->
        if ind = i  then List.append values v
            else v]

let parseState lines =
    let lines = Seq.rev lines |> Seq.skip 1
    let length = Seq.head lines |> Seq.length
    let emptyState = [for _ in 0 .. (length - 1)/4 -> []]
    let moves =
         [for line in lines do
            for i,v in Seq.indexed line do
                match v with
                | ' ' -> ()
                | '[' -> ()
                | ']' -> ()
                | v -> yield ((i - 1)/4,v)]
    let rec init state = function
        | (i,v)::tail ->
            let state = put [v] i state
            init state tail
        | _ -> state
    init emptyState moves

[<Puzzle(2022, 5)>]
let puzzle case (source:string) =
    let arrange crates =
        match case with
        | Case.A -> crates |> List.rev
        | Case.B -> crates

    let state, moves =
        match String.split $"{Environment.NewLine}{Environment.NewLine}" source with
        | [| s; m |] ->
            let state = s |> String.split Environment.NewLine |> parseState
            let moves = m |> String.split Environment.NewLine |> Seq.map parseMove |> Seq.toList
            state, moves
        | _ -> failwith "incorrect input"
    let rec run state = function
        | (quantity, fromInd, toInd)::tail ->
            let state, crates = extract fromInd quantity state
            let crates = arrange crates
            let state = put crates toInd state
            run state tail
        | _-> state
    let state = run state moves
    List.map (List.tryHead >> Option.defaultValue '.') state
    |> String.joinSeq ""







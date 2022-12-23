module AdventOfCode.Cases.Y2022.Day21
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

type Operation = | Add | Distract | Multiple | Divide

type Monkey = | Number of string*option<int64> | Act of string*((int64->int64->option<int64>)*string*string)

let parseCmd = function
    | "-" -> (-)
    | "+" -> (+)
    | "*" -> (*)
    | "/" -> (/)
    | _ -> failwith "Stupid monkey"

let parseLine src =
    match String.split " " src with
    | [| name; Int num |] -> Number (name.TrimEnd(':'), Some num)
    | [| name; fst; act; snd |] ->
        let act = parseCmd act
        Act (name.TrimEnd(':'), ((fun f s -> act f s |> Some), fst, snd))
    | _ -> failwith "unknown format"

let rec removeKeys map = function
    | h::rest ->
        let map = Map.remove h map
        removeKeys map rest
    | _ -> map

let rec addKeys map = function
    | (k, v)::rest ->
        let map = Map.add k v map
        addKeys map rest
    | _ -> map

let rec calculateRec numbers = function
    | map when Map.isEmpty map -> numbers, map
    | map ->
        let stepCalc =
             [for v in map do
                let n, (a, f, s) = v.Deconstruct()
                match Map.tryFind f numbers, Map.tryFind s numbers with
                | Some (Some f), Some (Some s) -> yield n, (a f s)
                | _, _ -> ()]
        match stepCalc with
        | [] -> numbers, map
        | _ ->
            let map = removeKeys map (List.map fst stepCalc)
            let numbers = addKeys numbers stepCalc
            calculateRec numbers map

let rec calculateB numbers acts iter =
    let numbers = Map.add "humn" (Some iter) numbers
    calculateRec numbers acts
    |> fst
    |> Map.find "root"
    |> function
        | Some v -> Some iter
        | None -> calculateB numbers acts (iter + 1L)

[<Puzzle(2022, 21)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.map parseLine |> Seq.toList

    let numbers = source
                 |> Seq.map (function | Number (n,i) -> Some (n,i) | _ -> None)
                 |> Seq.filter Option.isSome
                 |> Seq.map (Option.defaultValue ("",None))
                 |> Map

    let acts = source
                 |> Seq.map (function | Act (n,v) -> Some (n,v) | _ -> None)
                 |> Seq.filter Option.isSome
                 |> Seq.map (Option.defaultValue ("", ((fun f s -> None), "", "")))
                 |> Map

    match case with
    | Case.A ->
        calculateRec numbers acts
        |> fst
        |> Map.find "root"
    | Case.B ->
        let _, f, s = Map.find "root" acts
        let acts = Map.add "root" ((fun f s -> if f = s then Some f else None), f, s) acts
        let numbers = Map.remove "humn" numbers
        let numbers, acts = calculateRec numbers acts
        calculateB numbers acts 0L
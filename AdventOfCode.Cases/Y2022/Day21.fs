module AdventOfCode.Cases.Y2022.Day21
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Operation = | Add | Distract | Multiple | Divide

type Command = | Add | Sub | Div | Mul

type Monkey = | Number of string*int64 | Act of string*(Command*string*string)


let parseCmd = function
    | "-" -> Sub //(-)
    | "+" -> Add //(+)
    | "*" -> Mul //(*)
    | "/" -> Div //(/)
    | _ -> failwith "Stupid monkey"

let runAct = function
    | Add -> (+)
    | Sub -> (-)
    | Mul -> (*)
    | Div -> (/)

let parseLine src =
    match String.split " " src with
    | [| name; Int num |] -> Number (name.TrimEnd(':'), num)
    | [| name; fst; act; snd |] ->
        let act = parseCmd act
        Act (name.TrimEnd(':'), (act, fst, snd))
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
                | Some f, Some s-> yield n, (runAct a <| f <| s)
                | _, _ -> ()]
        match stepCalc with
        | [] -> numbers, map
        | _ ->
            let map = removeKeys map (List.map fst stepCalc)
            let numbers = addKeys numbers stepCalc
            calculateRec numbers map

let buildFolder acc k cur =
    let numbers, rest = acc
    let cmd, f, s = cur
    match Map.tryFind k numbers with
    | None ->
        let rest = Map.add k (cmd, f, s) rest
        numbers, rest
    | Some res ->
        match Map.tryFind f numbers, Map.tryFind s numbers with
        | Some v, _ when cmd = Add ->
            let known = Map.add s <| res - v <| numbers
            known, rest
        | Some v, _ when cmd = Sub ->
            let known = Map.add s <| v - res <| numbers
            known, rest
        | Some v, _ when cmd = Mul ->
            let known = Map.add s <| res/v <| numbers
            known, rest
        | Some v, _ when cmd = Div ->
            let known = Map.add s <| v/res <| numbers
            known, rest
        | _, Some v when cmd = Add ->
            let known = Map.add f <| res - v <| numbers
            known, rest
        | _, Some v when cmd = Sub ->
            let known = Map.add f <| v + res <| numbers
            known, rest
        | _, Some v when cmd = Mul ->
            let known = Map.add f <| res/v <| numbers
            known, rest
        | _, Some v when cmd = Div ->
            let known = Map.add f <| v*res <| numbers
            known, rest
        | v, vb ->
            let rest = Map.add k (cmd, f, s) rest
            numbers, rest

let rec calculateB numbers rest =
    let numbers, rest = Map.fold buildFolder (numbers, Map.empty) rest
    if Map.isEmpty rest then numbers
    else
        calculateB numbers rest

let numId (v:int64) = v

let numConst (v:int64) (_:int64) = v

[<Puzzle(2022, 21)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.map parseLine |> Seq.toList

    let numbers = source
                 |> Seq.fold (fun acc -> function | Number (n,v) -> (n,v)::acc | _ -> acc) []
                 |> Map

    let acts = source
                 |> Seq.fold (fun acc -> function | Act (n,v) -> (n,v)::acc | _ -> acc) []
                 |> Map

    match case with
    | Case.A ->
        calculateRec numbers acts
        |> fst
        |> Map.find "root"
    | Case.B ->
        let numbers = Map.remove "humn" numbers
        let numbers, acts = calculateRec numbers acts
        let _, f, s = Map.find "root" acts
        let acts = Map.remove "root" acts
        let numbers =
            match Map.tryFind f numbers with
            | None -> numbers
            | Some v -> Map.add s v numbers
        let numbers =
            match Map.tryFind s numbers with
            | None -> numbers
            | Some v -> Map.add f v numbers
        calculateB numbers acts
        |> Map.find "humn"
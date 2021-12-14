module AdventOfCode.Cases.Y2021.Day13

open System
open System.Collections.Generic
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

open AdventOfCode.Cases.Infrastructure.Parser

type Fold = | X of int | Y of int

let parseDot = String.split "," >> function
    | [| Int f; Int s |] -> f,s
    | _ -> failwith "incorrect dot"

let parseFold = String.split "=" >> function
    | [| "fold along x"; Int v |] -> X v
    | [| "fold along y"; Int v |] -> Y v
    | _ -> failwith "incorrect instruction"

let parse = String.split $"{Environment.NewLine}{Environment.NewLine}" >> function
    | [| dots; folds |] ->
        let dots = String.split $"{Environment.NewLine}" dots |> Seq.map parseDot
        let folds = String.split $"{Environment.NewLine}" folds |> Seq.map parseFold |> Seq.toList
        dots, folds
    | _ -> failwith "incorrect input"

let rec foldDot folds (x,y) =
    match folds with
    | [] -> (x,y)
    | X fold::rest when x < fold -> foldDot rest (x, y)
    | Y fold::rest when y < fold -> foldDot rest (x, y)
    | X fold::rest -> foldDot rest (2*fold - x, y)
    | Y fold::rest -> foldDot rest (x, 2*fold - y)
    | _ -> (x,y)

[<Puzzle(2021, 13)>]
let puzzle case (input:string) =
    let dots, folds = parse input
    let mapper =
        foldDot
        <| match case with
            | Case.A -> List.take 1 folds
            | Case.B -> folds
    let result =
        dots
        |> Seq.map mapper
        |> Set.ofSeq

    let size = folds |> List.map (function | X v -> v |_ -> 0) |> List.max
    let line src = [for i in 0..size-1 -> if Seq.contains i src then '#' else ' '] |> String.joinSeq ""
    let text =
        result
        |> Seq.groupBy snd// group line (by y)
        |> Seq.sortBy fst// sort by y
        |> Seq.map (snd >> Seq.map fst >> line)// join x to single line
        |> Seq.reduce (fun acc cur -> $"{acc}{Environment.NewLine}{cur}") // join with line break
    Set.count result, text

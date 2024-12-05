module AdventOfCode.Cases.Y2024.Day5
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let parsePages = String.split "," >> Array.map int >> Seq.indexed >> Seq.map (fun (i,v) -> v,i) >> Map.ofSeq

let parsePageSet = String.lines >> Array.map parsePages

let parseRule = String.split "|" >> function
    | [| Int f; Int s |] -> f,s
    | _ -> failwith "incorrect rule"

let parseRules = String.lines >> Seq.map parseRule >> Seq.toList
  
let parseSource = String.split $"{Environment.NewLine}{Environment.NewLine}" >> function
    | [| rules; pages |] -> (parseRules rules, parsePageSet pages)
    | _ -> failwith "incorrect source"
    
let rec validateA pages = function
    | [] -> true
    | (f,s)::rest ->
        match Map.tryFind f pages, Map.tryFind s pages with
        | Some i, Some j when i > j -> false
        | _ -> validateA pages rest
        
let getMiddle map =
    let cnt = Map.count map
    let middle = cnt / 2
    Map.findKey (fun _ v -> v = middle) map
        
let resultA rules pageSets = Seq.filter (fun s -> validateA s rules) pageSets |> Seq.sumBy getMiddle
  
let rec sortPages pages = function
    | [] -> pages
    | (f,s)::rest ->
        let pages =
            match Map.tryFind f pages, Map.tryFind s pages with
            | Some i, Some j when i > j ->
                pages |> Map.add f j |> Map.add s i
            | _ -> pages
        sortPages pages rest
   
let rec sortAllPages pages rules =
    let updatePages = sortPages pages rules
    if updatePages = pages then updatePages
    else sortAllPages updatePages rules
    
   
let resultB rules pageSets =
    Seq.filter (fun s -> validateA s rules |> not) pageSets
    |> Seq.map (fun s -> sortAllPages s rules) |> Seq.sumBy getMiddle

[<Puzzle(2024, 5)>]
let puzzle case (source:string) =
    source
    |> parseSource
    ||>
        match case with
        | Case.A -> resultA
        | Case.B -> resultB







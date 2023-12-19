module AdventOfCode.Cases.Y2023.Day19
open System
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let parseParts src =
    let getValue = Regex.Match (src,  "\{x=(?<x>\d+),m=(?<m>\d+),a=(?<a>\d+),s=(?<s>\d+)\}") |> Regex.groupValue
    (getValue "x" |> int64), (getValue "m" |> int64), (getValue "a" |> int64), (getValue "s" |> int64)

let parseFlowStep src =
    let getValue = Regex.Match (src,  "(?<prop>\w)(?<cmd>\D)(?<comp>\d+):(?<res>\w+)") |> Regex.groupValue
    (getValue "prop"), (getValue "cmd"), (getValue "comp" |> int64), (getValue "res")
    
let parseFlows src =
     match String.split "{" src with
     | [| name; flows |] ->
         let flows = flows |> String.trimChar '}' |> String.split "," |> Array.toList |> List.rev
         let fallBack = List.head flows
         
         let flows = flows |> List.tail |> List.rev |> List.map parseFlowStep
         name, (fallBack, flows)
     | v -> failwith $"unexpected string {v}"
     
let parseInputs = String.split $"{Environment.NewLine}{Environment.NewLine}" >> function
    | [| workFlows; parts |] ->
        let workFlows = workFlows |> String.split Environment.NewLine |> Array.toList |> List.map parseFlows |> Map
        let parts = parts |> String.split Environment.NewLine |> Array.toList  |> List.map parseParts        
        workFlows, parts
    | _ -> failwith "wtf"
 
let split  cmd border (g1, g2)=
    match cmd with
    | ">" ->
        if g2 <= border then None, Some (g1,g2)
        else if g1 > border then Some (g1, g2), None
        else Some (border + 1L, g2), Some (g1, border)
    | "<" ->
        if g1 >= border then None, Some (g1,g2)
        else if g2 < border then Some (g1, g2), None
        else Some (g1, border - 1L), Some (border, g2)
    | c -> failwith $"unknown command {c}"

let splitGaps src prop cmd border =
    let split = split cmd border
    let x, m, a, s = src
    match prop with
    | "x" ->
        let mtch, nonmtch = (split x)
        let mtch = Option.map (fun x -> x, m, a, s) mtch
        let nonmtch = Option.map (fun x -> x, m, a, s) nonmtch
        mtch, nonmtch
    | "m" ->
        let mtch, nonmtch = (split m)
        let mtch = Option.map (fun m -> x, m, a, s) mtch
        let nonmtch = Option.map (fun m -> x, m, a, s) nonmtch
        mtch, nonmtch
    | "a" ->
        let mtch, nonmtch = (split a)
        let mtch = Option.map (fun a -> x, m, a, s) mtch
        let nonmtch = Option.map (fun a -> x, m, a, s) nonmtch
        mtch, nonmtch
    | "s" ->
        let mtch, nonmtch = (split s)
        let mtch = Option.map (fun s -> x, m, a, s) mtch
        let nonmtch = Option.map (fun s -> x, m, a, s) nonmtch
        mtch, nonmtch
    | _ -> None, None
 
let rec countRec acc exit cur =
    if cur > exit then acc else
        let acc = cur + acc
        countRec acc exit <| cur + 1L
 
let countA v =
    let count (v1,v2) =
        let sum = countRec 0L v2 v1
        let cnt = v2 - v1 + 1L
        cnt, sum
    let x,m,a,s = v
    let cntx, sumx = count x
    let cntm, summ = count m
    let cnta, suma = count a
    let cnts, sums = count s
    (sumx * cntm*cnta*cnts)
    |> (+) <| (suma * cntm*cntx*cnts)
    |> (+) <| (summ * cntx*cnta*cnts)
    |> (+) <| (sums * cntm*cnta*cntx)
    
let countB v =
    let count (v1,v2) = v2 - v1 + 1L
    let x,m,a,s = v
    count x
    |> (*) <| count m
    |> (*) <| count a
    |> (*) <| count s

let rec foldFlows count acc tmp gap (fallBack, flows) =
    match flows with
    | [] ->
        match fallBack with
        | "A" ->
            let acc = acc + (count gap)
            acc, tmp
        | "R" -> acc, tmp
        | k ->
            let tmp = (k, gap)::tmp
            acc, tmp
    | h::rest ->        
        let prop, cmd, border, res = h
        let mtch, unmtch = splitGaps gap prop cmd border
        let acc = if res = "A" && Option.isSome mtch then acc + (mtch |> Option.get |> count) else acc
        let tmp = if res <> "R" && res <> "A" && Option.isSome mtch then (res, Option.get mtch)::tmp else tmp
        match unmtch with
        | None -> acc, tmp
        | Some g ->
            foldFlows count acc tmp g (fallBack, rest) 
            
let rec apply count acc gaps flowMap =
    let folder (acc, tmp) (k, gap) =
         let tgtFlow = match Map.tryFind k flowMap with | Some v -> v | None -> failwith $"not found {k}"
         foldFlows count acc tmp gap tgtFlow
    let acc, gaps = List.fold folder (acc, List.Empty) gaps
    if List.isEmpty gaps then acc else
        apply count acc gaps flowMap
        
        
[<Puzzle(2023, 19)>]
let puzzle case (source:string) =
    let workflows, parts = source |> parseInputs
    let gaps, count = 
        match case with
        | Case.A ->
            let gaps = parts |> List.map (fun (x,m,a,s) -> (x,x), (m,m), (a,a), (s,s)) |> List.map (fun v -> "in",v)
            gaps, countA
        | Case.B ->
            let gaps = ["in",((1L, 4000L), (1L, 4000L), (1L, 4000L), (1L, 4000L))]
            gaps, countB
    apply count 0L gaps workflows
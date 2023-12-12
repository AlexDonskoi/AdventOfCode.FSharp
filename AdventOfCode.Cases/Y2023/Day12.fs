module AdventOfCode.Cases.Y2023.Day12
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let parseSeq = String.split "," >> Seq.rev >> Seq.toList >> List.map int

let parseRow = String.split " " >> function
    | [| src; pat |] -> src |> Seq.rev |> Seq.toList, parseSeq pat
    | v -> failwith $"wtf {v}"

let size = 25

let rec groups acc cur =
    let head = List.head acc
    if cur = 0L then
        if head = 0 then List.tail acc else acc |> List.rev
    else
        let rest = List.tail acc
        let acc = if cur &&& 1L = 1L then (head+1)::rest else if head <> 0 then 0::acc else acc
        let cur = cur >>> 1
        groups acc cur
        
let rec maskMatch cur = function
    | [] when cur = 0L -> true
    | h::rest ->
        let notAllowed = if cur &&& 1L = 1L then '.' else '#'
        let cur = if cur = 0L then 0L else cur >>> 1
        h <> notAllowed && maskMatch cur rest
    | _ -> false

let rec count src step =
    let pat = groups [0] step
    let maskMatch = maskMatch step
    match Map.tryFind pat src with
        | Some arr ->
            arr |> List.fold (fun acc cur -> if maskMatch cur then acc + 1L else acc) 0L 
        | _ -> 0L   

let rec countRec src acc step =
    if step = 0L then acc else
        let acc = (+) acc <| count src step
        countRec src acc <| step - 1L   

let result = parseRow
    
[<Puzzle(2023, 12)>]
let puzzle case (source:seq<string>) =
    let source = source
              |> Seq.map parseRow
              |> Seq.map (fun (a,b)->b, a)
              |> Seq.fold (fun a (k,v) -> Map.change k (Option.defaultValue [] >> List.append [v] >> Some) a) Map.empty
              
    match case with
    | Case.A -> countRec source 0L <| pown 2L size - 1L
    | Case.B -> 0L
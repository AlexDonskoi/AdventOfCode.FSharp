module AdventOfCode.Cases.Y2023.Day20
open System
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let parseLine = String.split " -> " >> function
    [| src; dest |] ->
        let dest = String.split ", " dest |> Array.toList
        src, dest
    | _ -> failwith "wrong input"


type State = | Noop | Switch of bool | Multi of Map<string, bool>

let rec connect nodes links =function
    | [] -> nodes, links
    | (name, dest)::rest ->
        let act, name = 
            match Seq.head name with
            | '%' ->
                let name = Seq.tail name |> String.joinSeq ""
                Switch false, name
            | '&' ->
                let name = Seq.tail name |> String.joinSeq ""
                Multi Map.empty, name
            | _ -> Noop, name    
        let nodes = Map.add name (act, dest) nodes
        let links =
            dest
            |> List.fold (fun acc c -> Map.change c (Option.defaultValue [] >> List.append [name] >> Some) acc) links
        connect nodes links rest
 
let pulse nodes state (src, imp, cur) =
    let targets imp =
        Map.find cur nodes
        |> List.map (fun c -> cur,imp, c)
        
    match Map.tryFind cur state with
    | Some(Switch v) when imp |> not ->
        let imp = not v
        let targets = imp |> targets
        (Map.add cur (Switch imp) state), targets
    | Some(Multi map) ->
        let map = Map.add src imp map        
        let targets = Map.forall (fun _ v -> v) map |> not |> targets
        (Map.add cur (Multi map) state), targets
    | Some Noop -> state, (targets imp)
    | _ -> state,[]    
        
let rec pulseAll source getAcc acc state moves =
    if List.isEmpty moves then state, acc else
        let folder (state, moves) c =
            let state, mv = pulse source state c
            let moves = List.append moves mv
            state, moves
    
        let acc = getAcc acc moves
        let state, moves = moves |> List.fold folder (state, [])
        pulseAll source getAcc acc state moves
        
let rec runCountA source acc state step =
    let getAcc (h,l) moves =
        let h = moves |> List.filter (fun (_,c,_) -> c) |> List.length |> (+) h
        let l = moves |> List.filter (fun (_,c,_) -> not c) |> List.length |> (+) l
        h,l
    
    if step = 0 then acc ||> (*) else
        let button = ["button", false, "broadcaster"]
        let state, acc = pulseAll source getAcc acc state button
        runCountA source acc state <| step - 1
        
let rec runCountB source state step =
    let getAcc cnt moves =
        // kr, zs, kf, qk
        // get repetition for nodes connected t0 &_ -> rx
        if moves |> List.exists (fun (_,b,c) -> c = "zs" &&  not b)
            then
                cnt + 1
            else cnt
        
    let button = ["button", false, "broadcaster"]
    let state, acc = pulseAll source getAcc 0 state button
    if acc > 0
    then
        printfn $"%d{step}"
                
        runCountB source state <| step + 1L
    else
        runCountB source state <| step + 1L
        
[<Puzzle(2023, 20)>]
let puzzle case (source:seq<string>) =
    let nodes, links = source |> Seq.map parseLine |> Seq.toList |> connect Map.empty Map.empty
    let state =
        nodes
        |> Map.map (fun k v -> match fst v with | Multi _ -> Map.find k links |> List.map (fun c -> c,false) |> Map |> Multi | v -> v)
    let nodes =
        nodes
        |> Map.map (fun k -> snd)
    
    match case with
    | Case.A ->
        runCountA nodes (0, 0) state 1000 |> int64
    | Case.B -> runCountB nodes state 1L
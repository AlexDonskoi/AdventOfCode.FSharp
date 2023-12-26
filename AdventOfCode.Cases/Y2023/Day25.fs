module AdventOfCode.Cases.Y2023.Day25
open System
open System.Diagnostics
open System.Globalization
open System.Net.Sockets
open System.Numerics
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Threading.Tasks
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let parseLine = String.split ": " >> function
    | [| src; targets |] -> src, String.split " " targets
    | _ -> failwith "wtf"

let appendOption v = Option.defaultValue Set.empty >> Set.add v >> Some
    
let insertMap map k v =
    map
    |> Map.change k (appendOption v)
    |> Map.change v (appendOption k)
 
let rec getPathRec map current exit =
    let tmp = Set.minElement current
    let steps, point, visited = tmp
    let visited = Set.add point visited
    let current = Set.remove tmp current
    if point = exit then Some visited else
        let current =
            Map.tryFind point map
            |> Option.defaultValue Set.empty
            |> Set.filter (fun c -> Set.contains c visited |> not)
            |> Set.fold (fun acc c -> Set.add (steps + 1, c, visited) acc) current
        if Set.isEmpty current then None else
            getPathRec map current exit
            
let getPath map a b = getPathRec map <| Set.singleton (0, a, Set.empty) <| b

let removeConnections map a b = map |> Map.change a (Option.defaultValue Set.empty >> Set.remove b >> Some)  |> Map.change b (Option.defaultValue Set.empty >> Set.remove a >> Some) 
 
let rec checkConnections map a b step =
    let map = if step > 0 then removeConnections map a b else map
    match getPath map a b with
    | None -> Some map
    | Some visited when step = 0 -> None
    | Some visited ->
        let folder acc cur =
            match acc with
            | None -> cur ||> checkConnections map <| step - 1
            | v -> v
            
        visited |> Set.toList
        |> List.collect (fun c -> Map.find c map |> Set.map (fun s -> c,s) |> Set.toList)
        |> List.fold folder None

let rec getPoints map visited current =
    let visited = Set.union visited current
    let current = current |> Seq.collect (fun k -> Map.find k map) |> Set.ofSeq |> Set.difference <| visited
    if Set.isEmpty current then visited else
        getPoints map visited current
  
let getGroups map =
    let startPoint = Map.keys map |> Seq.head |> Set.singleton
    getPoints map Set.empty startPoint 


        
let rec searchRec map = function
    | [] -> None
    | (a,b)::rest ->
        match checkConnections map a b 3 with
        | Some map ->
            let grpCount = getGroups map |> Set.count
            let restCount = Map.count map - grpCount 
            Some (grpCount*restCount)
        | None -> searchRec map rest
    
[<Puzzle(2023, 25)>]
let puzzle case (source:seq<string>) =
    let folder map (s, arr) =
        Seq.fold (fun acc -> insertMap acc s) map arr
    let source = source |> Seq.map parseLine |> Seq.fold folder Map.empty
    
    let allPairs = Map.fold (fun acc k lst -> lst |> Seq.map (fun c -> k,c) |> Seq.toList |> List.append acc) List.empty source
    
    match case with
    | Case.A -> searchRec source allPairs
    | Case.B -> Some 0
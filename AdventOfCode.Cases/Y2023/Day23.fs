module AdventOfCode.Cases.Y2023.Day23
open System
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let toArray src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 '.'
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v]] |> ignore
    map
 
let getSteps src cache cur =
    match Map.tryFind cur cache with
    | Some (fin, brk, v) -> [fin,v;brk,1]
    | None ->     
        let size1, size2 = Array2D.sizes src
        let (i,j) = cur
        let v = src.[i,j]
        [
            if i > 0 && (v = '.' || v = '^')  then (i - 1), j
            if i < size1 && (v = '.' || v = 'v') then (i + 1), j
            if j > 0  && (v = '.' || v = '<') then i, j - 1
            if j < size2  && (v = '.' || v = '>') then i, j + 1
        ]
        |> List.filter (fun (i,j) -> src.[i,j] <> '#')
        |> List.map (fun c -> c, 1)
 
let rec getPath getSteps prev cur steps  =
    let moves = getSteps cur
    if List.length moves <> 2 then prev, steps else
        let move = List.find ((<>) prev) moves
        getPath getSteps cur move <| steps + 1
    
 
let rec searchEnd getSteps prev cur =
    let moves = getSteps cur
    if List.length moves <> 2 then cur,prev else
        let move = List.find ((<>) prev) moves
        searchEnd getSteps cur move
   
let fastForward getSteps acc i j v =
    if v = '#' then acc else
        let moves = getSteps (i,j)
        if List.length moves <> 2 then acc else
            let brk, start = searchEnd getSteps (i,j) <| List.head moves
            //let dir = moves |> List.filter ((<>) dir) |> List.head
            let fin, steps = getPath getSteps brk start -1
            let acc =
                if steps <= 1 then acc else
                    acc |> Map.add start (fin, brk, steps)
            
            let brk, start = searchEnd getSteps (i,j) <| List.last moves
            //let dir = moves |> List.filter ((<>) dir) |> List.head
            let fin, steps = getPath getSteps brk start -1
            let acc =
                if steps <= 1 then acc else
                    acc |> Map.add start (fin, brk, steps)
            acc          
    
let fillCache getSteps = Array2D.foldi (fastForward getSteps) Map.empty
    
let rec walk getSteps cache finish res states =
    let minState = Set.minElement states
    let v, cur, visited = minState
    let states = Set.remove minState states
    let visited = Set.add cur visited
    
    let folder visited acc (c,adj) = if Set.contains c visited then acc else Set.add (v + adj, c, visited) acc
    let folder = folder visited
    let states = if finish = cur then states else getSteps cur |> List.fold folder states
    let res = if finish = cur then max v res else res
    if Set.isEmpty states then res else
        walk getSteps cache finish res states
    
    
[<Puzzle(2023, 23)>]
let puzzle case (source:seq<string>) =
    let source = toArray source
    let size1, size2 = Array2D.sizes source
    
    let res = Array2D.foldi (fun acc i j v -> if v <> '#' && getSteps source Map.empty (i,j) |> List.length |> (<) 3 then acc + 1 else acc) 0 source
    
    let source =
        match case with
        | Case.A -> source
        | Case.B ->
            Array2D.iteri (fun i j v -> if v <> '#' then source.[i,j]<- '.' else ()) source
            source
    let start = source.[0,*] |> Array.findIndex ((=) '.')
    let finish = source.[size1,*] |> Array.findIndex ((=) '.')
    let getNoCache = getSteps source Map.empty >> List.map fst
    let cache = fillCache getNoCache source
    let getSteps = getSteps source cache
    walk getSteps cache <| (size1,finish) <| 0 <| Set.singleton (0, (0,start), Set.empty)
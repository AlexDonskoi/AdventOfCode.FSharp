module AdventOfCode.Cases.Y2023.Day21
open System
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let toArray src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 '.'
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v]] |> ignore
    map  

let next field (i,j) =
    let size1, size2 = Array2D.sizes field
    [
        if i > 0 then i - 1,j
        if j > 0 then i, j - 1
        if i < size1 then i + 1, j
        if j < size2 then i, j + 1
    ]
    |> List.filter (fun (i,j) -> field.[i,j] <> '#')

let rec steps source visited current =
    if Set.isEmpty current then visited else
        let cur = Set.minElement current
        let current = Set.remove cur current
        let step, point = cur
        if Map.containsKey point visited then steps source visited current else
            let visited = Map.add point step visited
            let current =
                next source point
                |> List.fold (fun acc p -> Set.add (step + 1L, p) acc) current
              
            steps source visited current     
   
type Side = | Left | Right | Top | Bottom

let filterReachable limit = Map.filter (fun _ v -> v <= limit)

let getSides size1 size2 points =
    let left = Map.fold (fun acc (i,j) v -> if j = 0 then Map.add (i,j) v acc else acc) Map.empty points
    let right = Map.fold (fun acc (i,j) v -> if j = size2 then Map.add (i,j) v acc else acc) Map.empty points
    let top = Map.fold (fun acc (i,j) v -> if i = 0 then Map.add (i,j) v acc else acc) Map.empty points
    let bottom = Map.fold (fun acc (i,j) v -> if i = size1 then Map.add (i,j) v acc else acc) Map.empty points
    [
        if Map.isEmpty left |> not then Left, left
        if Map.isEmpty right |> not then Right, right
        if Map.isEmpty top |> not then Top, top
        if Map.isEmpty bottom |> not then Bottom, bottom
    ]

let calculate (size1, size2) cache getSides filter count fields (acc, newFields) cur =
    let mutable map = Map.empty
    let change v = Option.defaultValue Int64.MaxValue >> min v >> Some
    
    match cur with
    | pi, pj when pi >= 0 && pj >= 0 ->
        let initPoints = fields |> Map.tryFind (Right, (pi, pj - 1)) |> Option.defaultValue Map.empty
        for item in initPoints do
            let i,_ = item.Key
            let adj = item.Value + 1L
            let moves = Map.find (i, 0) cache
            map<- Map.fold (fun acc k v -> Map.change k (change (v + adj)) acc) map moves
            
        let initPoints = fields |> Map.tryFind (Bottom, (pi - 1, pj)) |> Option.defaultValue Map.empty
        for item in initPoints do
            let _,j = item.Key
            let adj = item.Value + 1L
            let moves = Map.find (0, j) cache
            map<- Map.fold (fun acc k v -> Map.change k (change (v + adj)) acc) map moves
            
    | pi, pj when pi >= 0 && pj <= 0 ->
        let initPoints = fields |> Map.tryFind (Left, (pi, pj + 1)) |> Option.defaultValue Map.empty
        for item in initPoints do
            let i,_ = item.Key
            let adj = item.Value + 1L
            let moves = Map.find (i, size2) cache
            map<- Map.fold (fun acc k v -> Map.change k (change (v + adj)) acc) map moves
            
        let initPoints = fields |> Map.tryFind (Bottom, (pi - 1, pj)) |> Option.defaultValue Map.empty
        for item in initPoints do
            let _,j = item.Key
            let adj = item.Value + 1L
            let moves = Map.find (0, j) cache
            map<- Map.fold (fun acc k v -> Map.change k (change (v + adj)) acc) map moves
            
    | pi, pj when pi <= 0 && pj >= 0 ->
        let initPoints = fields |> Map.tryFind (Right, (pi, pj - 1)) |> Option.defaultValue Map.empty
        for item in initPoints do
            let i,_ = item.Key
            let adj = item.Value + 1L
            let moves = Map.find (i, 0) cache
            map<- Map.fold (fun acc k v -> Map.change k (change (v + adj)) acc) map moves
            
        let initPoints = fields |> Map.tryFind (Top, (pi + 1, pj)) |> Option.defaultValue Map.empty
        for item in initPoints do
            let _,j = item.Key
            let adj = item.Value + 1L
            let moves = Map.find (size1, j) cache
            map<- Map.fold (fun acc k v -> Map.change k (change (v + adj)) acc) map moves
            
    | pi, pj when pi <= 0 && pj <= 0 ->
        let initPoints = fields |> Map.tryFind (Left, (pi, pj + 1)) |> Option.defaultValue Map.empty
        for item in initPoints do
            let i,_ = item.Key
            let adj = item.Value + 1L
            let moves = Map.find (i, size2) cache
            map<- Map.fold (fun acc k v -> Map.change k (change (v + adj)) acc) map moves
            
        let initPoints = fields |> Map.tryFind (Top, (pi + 1, pj)) |> Option.defaultValue Map.empty
        for item in initPoints do
            let _,j = item.Key
            let adj = item.Value + 1L
            let moves = Map.find (size1, j) cache
            map<- Map.fold (fun acc k v -> Map.change k (change (v + adj)) acc) map moves
    | _ -> failwith "wtf"
    
    map<- filter map
    let acc = map |> count |> (+) acc
    let newFields =
         getSides map
         |> List.collect (fun (side, points) -> [(side, cur), points])
         |> List.fold (fun acc (k, v) -> Map.add k v acc) newFields
    acc, newFields

let moves (i,j) =
    [
        if i >= 0 then (i + 1, j)
        if i <= 0 then (i - 1, j)
        if j >= 0 then (i, j + 1)
        if j <= 0 then (i, j - 1)
    ]
    
 
let rec run sizes cache getSides filter count acc points =
    if Map.isEmpty points then acc else
        let acc, points =
            points
            |> Map.keys
            |> Seq.collect (snd >> moves)
            |> Set.ofSeq
            |> Seq.fold (calculate sizes cache getSides filter count points) (acc, Map.empty)
        run sizes cache getSides filter count acc points
        
let getCount limit =
    let rmdr = limit % 2L
    Map.fold (fun acc k v -> if v % 2L = rmdr then acc + 1L else acc) 0L
        
[<Puzzle(2023, 21)>]
let puzzle case (source:seq<string>) =
    let source = source |> toArray
    let start = Array2D.foldi (fun acc i j v -> if v = 'S' then Set.add (0L, (i,j)) acc else acc) Set.empty source
    let size1, size2 = Array2D.sizes source    
    
    let init = steps source Map.empty start
    match case with
        | Case.A -> init |> filterReachable 64L |> Map.count |> int64
        | Case.B ->
            // let limit = 26501365L
            //
            // let cache =
            //     [
            //         yield! [0..size1] |> List.collect (fun i -> [i,0; i,size2])
            //         yield! [0..size2] |> List.collect (fun j -> [0,j; size1,j])
            //     ]
            //     |> List.map (fun start -> start, (steps source Map.empty <| Set.singleton (0,start) |> ))
            //     |> Map
            //
            // let getSides = getSides size1 size2
            // let reachable = filterReachable limit
            // let init = reachable init
            // let getCount = getCount limit
            // let initCnt = getCount init
            // let initSet = init |> getSides |> List.collect (fun (side, points) -> [(side, (0,0)), points]) |> Map
            // run (size1, size2) cache getSides reachable getCount initCnt initSet
            for i in 0L..26501365L do
                for j in 0L..26501365L do
                    ()
            0L        
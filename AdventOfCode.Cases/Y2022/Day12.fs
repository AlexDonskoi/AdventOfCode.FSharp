module AdventOfCode.Cases.Y2022.Day12
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
open Microsoft.Win32.SafeHandles

type Point =
    | Visited of int
    | Unvisited of int
    | Finish

let rec move map road =
    let cur = Set.minElement road
    let (path, (x,y)) = cur
    let sizex, sizey = Array2D.sizes map
    let curVal =
        match map[x, y] with
        | Visited v -> v
        | Finish -> int 'z'
        | _ -> failwith "wtf"
        
    let road = Set.remove cur road
    let next =
        [
            if x > 0 then yield (x - 1, y)
            if x < sizex then yield (x + 1, y)
            if y > 0 then yield (x, y - 1)
            if y < sizey then yield (x, y + 1)
        ]
        |> List.map (fun (i,j) -> (i,j), map[i,j])
    
    let filter = snd >> function
        | Unvisited v -> v >= curVal - 1
        | Finish -> int 'a' >= curVal - 1
        | _ -> false
    let next = next |> List.filter filter
    
    
    match List.tryFind (snd >> (=) Finish) next with
    | Some v -> path + 1
    | None ->
            let rec updateRecursive map road path = function
                | ((i,j), Unvisited v)::tail ->
                    // Console.Write((path + 1, (i,j)))
                    // Console.Write("    ")
                    // Console.Write((x,y))
                    // Console.WriteLine()
                    let road = road |> Set.add (path + 1, (i,j))
                    Array2D.set map i j  (Visited v)
                    updateRecursive map road path tail
                | _ -> map, road
            let map, road = updateRecursive map road path next
            move map road    
        
[<Puzzle(2022, 12)>]
let puzzle case (source:seq<string>) =
    let mapperA = function
        | 'E' -> 'z' |> int |> Visited
        | 'S' -> Finish
        | v -> int v |> Unvisited
    
    let mapperB = function
        | 'E' -> 'z' |> int |> Visited
        | 'S' -> Finish
        | 'a' -> Finish
        | v -> int v |> Unvisited
           
    let mapper =
        match case with
        | Case.A -> mapperA
        | Case.B -> mapperB    
        
    let source =
        source
        |> array2D
        |> Array2D.map mapper
    
    let folder acc i j cur=
        match i, j, cur with
        | _, _, Visited _ -> (i, j)
        | _, _, Unvisited _ -> acc
        | _ -> acc
    let start = Array2D.foldi folder (0, 0) source
    
    let roadSet = Set.singleton (0, start)
    move source roadSet



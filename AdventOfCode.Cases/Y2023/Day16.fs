module AdventOfCode.Cases.Y2023.Day16
open System
open System.Diagnostics
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

type Dir = | Up | Down | Left | Right

let nextDir cur cell =
    let (i,j), dir = cur
    match dir, cell with
    | Up, '\\' -> [(i, j - 1), Left]
    | Down, '\\' -> [(i, j + 1), Right]
    | Left, '\\' -> [(i - 1, j), Up]
    | Right, '\\' -> [(i + 1, j), Down]

    | Up, '/' -> [(i, j + 1), Right]
    | Down, '/' -> [(i, j - 1), Left]
    | Left, '/' -> [(i + 1, j), Down]
    | Right, '/' -> [(i - 1, j), Up]
   
    | Up, '|' -> [(i - 1, j), Up]
    | Down, '|' -> [(i + 1, j), Down]
    | Left, '|' -> [(i + 1, j), Down;(i - 1, j), Up]
    | Right, '|' -> [(i + 1, j), Down;(i - 1, j), Up]
   
    | Up, '-' -> [(i, j - 1), Left; (i, j + 1), Right]
    | Down, '-' -> [(i, j - 1), Left; (i, j + 1), Right]
    | Left, '-' -> [(i, j - 1), Left]
    | Right, '-' -> [(i, j + 1), Right]
    
    | Up, '.' -> [(i - 1, j), Up]
    | Down, '.' -> [(i + 1, j), Down]
    | Left, '.' -> [(i, j - 1), Left]
    | Right, '.' -> [(i, j + 1), Right]
    
    | _ -> []
   
let getValue src i j =
    let size1, size2 = Array2D.sizes src
    if i < 0 || j < 0 || i > size1 || j > size2 then None else Some src.[i,j]

let beamMove src acc cur =
    let ((i,j), _) = cur
    let (curBeams, visited, cells) = acc
    if Set.contains cur visited then acc else
        match getValue src i j with
        | None -> acc
        | Some cell ->
            let newBeams = nextDir cur cell
            
            let curBeams = List.fold (fun acc c -> Set.add c acc) curBeams newBeams
            let cells = Set.add (i,j) cells
            let visited = Set.add cur visited
            curBeams, visited, cells
           
           
let rec move src (curBeams, visited, cells) =
    if Set.isEmpty curBeams then cells else        
        let res = Set.fold <| beamMove src <| (Set.empty, visited, cells) <| curBeams
        move src res
 
let count source start =
    let startPoint = Set.singleton start
    let cells = move source (startPoint, Set.empty, Set.empty)
    Set.count cells

[<Puzzle(2023, 16)>]
let puzzle case (source:seq<string>) =
    let source = source |> toArray
    let startPoint = Set.singleton ((0,0),Right)
    match case with
    | Case.A -> count source ((0,0),Right)
        
    | Case.B -> 
        let size1, size2 = Array2D.sizes source
        [
            for i in 0..size1 do
                yield (i,0), Right
                yield (i,size2), Left
            for j in 0..size2 do
                yield (0,j), Down
                yield (size1, j), Up
        ]
        |> List.map (count source) |> List.max
    
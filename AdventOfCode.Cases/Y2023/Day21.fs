module AdventOfCode.Cases.Y2023.Day21
open System
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
type Item = | Rock | Space of int64    
let toArray src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 '.'
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v]] |> ignore
    map  

let nextStepsA field (i,j) =
    let size1, size2 = Array2D.sizes field
    [
        if i > 0 then i - 1,j
        if j > 0 then i, j - 1
        if i < size1 then i + 1, j
        if j < size2 then i, j + 1
    ]
    |> List.filter (fun (i,j) -> field.[i,j] <> '#')
 
let nextStepsB field (i,j) =
    let size1, size2 = Array2D.sizes field
    let size1 = size1 + 1
    let size2 = size2 + 1
    [
        i - 1,j
        i, j - 1
        i + 1, j
        i, j + 1
    ]
    |> List.filter (fun (i,j) -> field.[((i % size1) + size1)%size1 , ((j % size2) + size2)%size2] <> '#') 
 
let rec move nextSteps positions step =
    if step = 0 then positions else
        let folder acc cur =
            nextSteps cur
            |> List.fold (fun acc c -> Set.add c acc) acc
        let positions = Set.fold folder Set.empty positions    
        move nextSteps positions <| step - 1
        
[<Puzzle(2023, 21)>]
let puzzle case (source:seq<string>) =
    let field = source |> toArray
    let copyField = source |> toArray
    let start = Array2D.foldi (fun acc i j v -> if v = 'S' then (i,j) else acc) (0,0) field |> Set.singleton
    match case with
    | Case.A ->
        move (nextStepsA field) start 64 |> Set.count
    | Case.B -> 
        move (nextStepsB field) start 26501365 |> Set.count
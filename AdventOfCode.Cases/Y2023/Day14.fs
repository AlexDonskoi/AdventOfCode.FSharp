module AdventOfCode.Cases.Y2023.Day14
open System
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
    
let parseInput = String.split Environment.NewLine >> toArray

type Move = | North | South | West | East


let isFree src i j =
    let size1, size2 = Array2D.sizes src
    i >= 0 && i <= size1 && j <= size2 && j >= 0 && src.[i,j]='.'

let rec fall src dir i j =
    let tgti, tgtj =
        match dir with
        | North -> i - 1,j
        | South -> i + 1, j
        | West -> i,j - 1
        | East -> i, j + 1
        
    if isFree src tgti tgtj |> not then () else
        Array2D.set src tgti tgtj 'O'
        Array2D.set src i j '.'
        fall src dir tgti tgtj

let rec tryFall src dir i j =
    if Array2D.get src i j <> 'O' then () else
        fall src dir i j  

let move src dir =
    let size1, size2 = Array2D.sizes src
    match dir with
    | North ->
        for col in 0..size2 do
           for row in 0..size1 do
                tryFall src North row col
    | South -> 
        for col in 0..size2 do
            for row in size1 .. -1 .. 0 do
                tryFall src South row col
    | West ->
        for col in 0..size2 do
            for row in 0..size1 do
                tryFall src West row col
    | East ->
        for col in size2 .. -1 .. 0 do
            for row in 0..size1 do
                tryFall src East row col
    src

let pushTrack track src cycle =
    let folder acc i j v =
        if v = 'O' then Set.add (i,j) acc else acc
    let cur = Array2D.foldi folder Set.empty src
    match Map.tryFind cur track with
    | None -> None, Map.add cur cycle track
    | Some v -> Some v, track


let rec moveB src track cycle =
    if cycle = 0L then src
    else
        match pushTrack track src cycle with
        | Some v, _  ->
            let cycle = cycle % (v - cycle)
            moveB src Map.empty cycle
        | None, track ->
            let src =
                move src North
                |> move <| West
                |> move <| South
                |> move <| East
            moveB src track <| cycle - 1L

let count src =
    let size1, _ = Array2D.sizes src
    let folder acc i j v =
        if v = 'O' then size1 + 1 - i else 0
        |> (+) acc
    Array2D.foldi folder 0 src
    
[<Puzzle(2023, 14)>]
let puzzle case (source:string) =
    let source =
        source
        |> parseInput
    match case with
    | Case.A -> move source North
    | Case.B ->
        moveB source Map.empty 1000000000L
    |> count
        
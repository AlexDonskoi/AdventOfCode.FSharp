module AdventOfCode.Cases.Y2023.Day17
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
    let map = Array2D.create size1 size2 0
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v |> string |> int ]] |> ignore
    map  

type Dir = | Up | Down | Left | Right | No

let canTurn state cur =
    match state, cur with
    | Up::_, Down -> false
    | Down::_, Up -> false
    | Left::_, Right -> false
    | Right::_, Left -> false
    | _ -> true

let isValidA _ _ state cur =
    match state with
    | v1::v2::v3::_  when v1 = v2 && v2 = v3 && v3 = cur -> None
    | _ ->
        let state = cur::state
        if List.length state > 3 then List.take 3 state else state
        |> Some
        
let isValidB src (i, j) state cur =    
    let size1, size2 = Array2D.sizes src
    match state with
    | v1::v2::v3::v4::v5::v6::v7::v8::v9::v10::_
        when v1 = v2 && v2 = v3 && v3 = v4 && v4 = v5 && v5 = v6 && v6 = v7 && v7 = v8 && v8 = v9 && v9 = v10 && v10 = cur -> None
    | v1::v2::v3::v4::_ when cur <> v1 && v1 = v2 && v2 = v3 && v3 = v4 ->
        let hasSpace =
            match cur with
            | _ when v1 = cur -> true
            | Up -> i > 3
            | Down -> i + 3 <= size1
            | Left -> j > 3        
            | Right -> j + 3 <= size2
            | _ -> false
        if hasSpace |> not then None else
                       
            let state = [cur]
            if List.length state > 10 then List.take 10 state else state
            |> Some
    | [] ->
        let state = cur::state
        if List.length state > 10 then List.take 10 state else state
        |> Some
    | [v1] when v1 = cur ->
        let state = cur::state
        if List.length state > 10 then List.take 10 state else state
        |> Some
    | [v1;v2] when v1 = v2 && v2 = cur ->
        let state = cur::state
        if List.length state > 10 then List.take 10 state else state
        |> Some
    | [v1;v2;v3] when v1 = v2 && v2 = v3 && v3 = cur ->
        let state = cur::state
        if List.length state > 10 then List.take 10 state else state
        |> Some
    | v1::v2::v3::_ when v1 = v2 && v2 = v3 && v3 = cur ->
        let state = cur::state
        if List.length state > 10 then List.take 10 state else state
        |> Some
    
    | _ -> None    

let rec move src validate visited steps =
    //printfn $"%A{steps}"
    let size1, size2 = Array2D.sizes src
    let minEl = Set.minElement steps
    let (v, (i,j), state) = minEl
    let steps = Set.remove minEl steps
    if i = size1 && j = size2 then v else
        
        let folder (accSteps, accVisited) (k, dir) =
            match validate src k state dir with
            | None -> (accSteps, accVisited)
            | Some st ->
                if Set.contains (k, st) accVisited then accSteps, accVisited else
                    let v = k ||> Array2D.get src |> (+) v
                    let accSteps = accSteps |> Set.add (v,k,st)
                    let accVisited = Set.add (k,st) accVisited
                    accSteps, accVisited
                
        let (next, visited) =
            [
                if i > 0 then yield ((i - 1, j), Up)
                if i < size1 then yield ((i + 1, j), Down)
                if j > 0 then yield ((i, j - 1), Left)
                if j < size2 then yield ((i, j + 1), Right)
            ]
            |> List.filter (snd >> canTurn state)
            |> List.fold folder (steps, visited)
        
        move src validate visited next
    
    

[<Puzzle(2023, 17)>]
let puzzle case (source:seq<string>) =
    let source = source |> toArray
    let validate =
        match case with
        | Case.A -> isValidA
        | Case.B -> isValidB
    let start = [(0, (0,0), [Right]); (0, (0,0), [Down])] |> Set.ofList
    let visited = [((0,0), [Right]); ((0,0), [Right])] |> Set.ofList
    move source validate visited start
    
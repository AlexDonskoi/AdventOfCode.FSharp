module AdventOfCode.Cases.Y2024.Day6
open System
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Core

type Direction = Up | Down | Left | Right

let withStart src =
    let start = Array2D.foldi (fun acc i j v -> if v = '^' then (i,j) else acc) (0,0) src
    (Up, start), src
    
let nextStep src cur =
    let maxI = Array2D.length1 src - 1
    let maxJ = Array2D.length2 src - 1
    let (dir, (i,j)) = cur
    let nextij =
        match dir with
        | Up when i > 0 -> (i - 1, j) |> Some
        | Down when i < maxI -> (i + 1, j) |> Some
        | Left when j > 0-> (i, j - 1) |> Some
        | Right when j < maxJ -> (i, j + 1) |> Some
        | _ -> None
    match nextij with
    | None -> None
    | Some nextij ->
        if nextij ||> Array2D.get src <> '#' then (dir, nextij)
        else
            let turn =
                match dir with
                | Up -> Right
                | Down -> Left
                | Left -> Up
                | Right -> Down
            (turn, (i,j))
        |> Some    
            
let rec moveA src cur visited =
    let visited = cur |> snd |> Set.add <| visited
    match nextStep src cur with
    | None -> visited
    | Some mv -> moveA src mv visited
    
let resultA start src = moveA src start Set.empty |> Set.count

let rec isLoop src cur visited =
    if (Set.contains cur visited) then true
    else
        let visited = cur |> Set.add <| visited
        match nextStep src cur with
        | None -> false
        | Some mv -> isLoop src mv visited
    

let resultB start src =
     let folder acc i j v =
         if v <> '.' then acc
         else
             Array2D.set src i j '#'
             let indLoop = isLoop src start Set.empty
             src[i,j]<-'.'
             if indLoop then acc + 1 else acc
     Array2D.foldi folder 0 src

[<Puzzle(2024, 6)>]
let puzzle case (source:seq<string>) =
    source
    |> array2D
    |> withStart
    ||>
        match case with
        | Case.A -> resultA
        | Case.B -> resultB







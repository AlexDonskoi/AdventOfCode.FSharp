module AdventOfCode.Cases.Y2024.Day4
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Core
  
//let incrementKey k adj = Map.change k (function | Some v -> Some (v + adj) | None -> Some adj)

type Direction = Up | Down | Left | Right | UpLeft | UpRight | DownLeft | DownRight
let allDirections = [Up; Down; Left; Right; UpLeft; UpRight; DownLeft; DownRight]

let getX =
    let allDirections c = [Up,c; Down,c; Left,c; Right,c; UpLeft,c; UpRight,c; DownLeft,c; DownRight,c]
    Array2D.foldi (fun acc i j v -> if v = 'X' then (i,j)::acc else acc) []
    >> List.collect allDirections

let nexti (i,j) = function
    | Up -> (i - 1, j)
    | Down -> (i + 1, j)
    | Left -> (i, j - 1)
    | Right -> (i, j + 1)
    | UpLeft -> (i - 1, j - 1)
    | UpRight -> (i - 1, j + 1)
    | DownLeft -> (i + 1, j - 1)
    | DownRight -> (i + 1, j + 1)
    
let next src ij dir =
    let maxI = Array2D.length1 src - 1
    let maxJ = Array2D.length2 src - 1
    let (i,j) = nexti ij dir
    if i >= 0 && i <= maxI && j >= 0 && j <= maxJ
    then Some ((dir,(i,j)),src.[i,j])
    else None

let resultA src =
    let next = next src
    src
    |> getX
    |> List.fold (fun acc (dir,ij) -> match next ij dir with | Some (v,'M') -> v::acc | _ -> acc) []
    |> List.fold (fun acc (dir,ij) -> match next ij dir with | Some (v,'A') -> v::acc | _ -> acc) []
    |> List.fold (fun acc (dir,ij) -> match next ij dir with | Some (v,'S') -> v::acc | _ -> acc) []
    |> List.length
   



let resultB src = 
    let maxI = Array2D.length1 src - 1
    let maxJ = Array2D.length2 src - 1
    let mutable cnt = 0
    for i in 1 .. maxI - 1 do
        for j in 1 .. maxJ - 1 do
            let x = src.[i,j] = 'A' && ((src.[i - 1,j - 1] = 'M' && (src.[i + 1,j + 1] = 'S')) || (src.[i - 1,j - 1] = 'S' && (src.[i + 1,j + 1] = 'M')))
                    && ((src.[i - 1,j + 1] = 'M' && (src.[i + 1,j - 1] = 'S'))|| (src.[i - 1,j + 1] = 'S' && (src.[i + 1,j - 1] = 'M')))
            if x then cnt<-cnt+1 else ()
    cnt 

[<Puzzle(2024, 4)>]
let puzzle case (source:seq<string>) =
    source
    |> array2D   
    |>
        match case with
        | Case.A -> resultA
        | Case.B -> resultB







module AdventOfCode.Cases.Y2021.Day15

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let parse: char[,]->int[,] = Array2D.map (fun v -> int v - 48)

let adjacent size i j =
    [
        if i >= size - 1 then () else yield i + 1,j
        if i <= 0 then () else yield i - 1,j
        if j >= size - 1 then () else yield i,j + 1
        if j <= 0 then () else yield i,j - 1
    ]

let next adjacents source =
    let listFolder acc cur =
        if Set.exists (fun (_, p) -> snd cur = p) acc then acc
                else Set.add cur acc
    adjacents |> List.fold listFolder source

let risk cur src (i,j) =
    let normalize v = (v - 1) % 9 + 1
    let size = src |> Array2D.length1
    Array2D.get src <| i % size <| j % size
    |> (+) (i / size)
    |> normalize
    |> (+) (j/size)
    |> normalize
    |> (+) cur, (i,j)

let rec foldRecursive adjacent target state visited =
    let w,p = Set.minElement state
    if p = target then w
    else
        let state = Set.remove (w,p) state
        let state =
            adjacent (w,p)
            |> List.filter (fun (_, p) -> Set.contains p visited |> not)
            |> next
            <| state
        let visited = Set.add p visited
        foldRecursive adjacent target state visited

let run src scale =
    let size = src |> Array2D.length1 |> (*) scale
    let adjacent size (w, (i,j)) =
        adjacent size i j |> List.map (risk w src)
    foldRecursive <| adjacent size <| (size - 1, size - 1) <| Set [0,(0,0)] <| Set.empty

[<Puzzle(2021, 15)>]
let puzzle case (input:seq<string>) =
    input
    |> array2D
    |> parse
    |> run
    <| match case with
        | Case.A -> 1
        | Case.B -> 5

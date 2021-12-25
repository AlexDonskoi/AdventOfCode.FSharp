module AdventOfCode.Cases.Y2021.Day25

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Herd = | East | South
type State = | Moved of Herd | Wait of Herd

let parse = function | '>' -> East |> Wait |> Some | 'v' -> South |> Wait |> Some | _ -> None


let moveEast arr =
    let _, size2 = Array2D.sizes arr
    let move i j = function
        | Some (Wait East) ->
            let tgt = if j = size2 then 0 else j + 1
            if Array2D.get arr i tgt = None then
                East |> Moved |> Some |> Array2D.set arr i tgt
            else ()
        | _ ->()
    let clear i j = function
        | Some (Moved East) ->
            let tgt = if j = 0 then size2 else j - 1
            Array2D.set arr i tgt None
        | _ ->()
    Array2D.iteri move arr
    Array2D.iteri clear arr

let moveSouth arr =
    let size1, _ = Array2D.sizes arr
    let move i j = function
        | Some (Wait South) ->
            let tgt = if i = size1 then 0 else i + 1
            if Array2D.get arr tgt j = None then
                South |> Moved |> Some |> Array2D.set arr tgt j
            else ()
        | _ ->()
    let clear i j = function
        | Some (Moved South) ->
            let tgt = if i = 0 then size1 else i - 1
            Array2D.set arr tgt j None
        | _ ->()
    Array2D.iteri move arr
    Array2D.iteri clear arr

let reset arr =
    let size1, size2 = Array2D.sizes arr
    let foldItem arr i acc j =
        match Array2D.get arr i j with
        | Some (Moved h) ->
            h |> Wait |> Some |> Array2D.set arr i j
            true
        |_-> acc
    let folder arr cols acc row =
        List.fold (foldItem arr row) acc cols
    List.fold (folder arr [0..size2]) false [0..size1]


let rec move cnt arr =
    moveEast arr
    moveSouth arr
    if reset arr then move (cnt + 1) arr  else cnt

[<Puzzle(2021, 25)>]
let puzzle case (input:seq<string>) =
    input
    |> Seq.map (Seq.map parse)
    |> array2D
    |>
    match case with
    | Case.A -> move 1
    | Case.B -> fun _ -> 0

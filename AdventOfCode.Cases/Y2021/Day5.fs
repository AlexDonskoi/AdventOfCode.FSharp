module AdventOfCode.Cases.Y2021.Day5

open AdventOfCode.Cases.Infrastructure

type Point = { X: int; Y: int }
type Line = { Start: Point; Stop: Point }

let point src =
    match String.split "," src with
    | [| x; y |] -> { Point.X = int x; Y = int y }
    | _ -> failwith "incorrect point format"

let line src =
    match String.split " -> " src with
    | [| start; stop |] -> { Line.Start = point start; Stop = point stop }
    | _ -> failwith "incorrect line format"

let nonDiagonal (src:Line) = src.Start.X = src.Stop.X || src.Start.Y = src.Stop.Y

let range src  =
    let range start stop = if start < stop then [start .. stop] else [start .. -1 .. stop]
    let {Line.Start = start; Stop = stop } = src
    let rangeX = range start.X stop.X
    let rangeY = range start.Y stop.Y

    if List.length rangeX <> List.length rangeY
    then
        [for i in rangeX do for j in rangeY -> (i,j)]
    else
        List.map2 (fun i j -> (i,j)) rangeX rangeY

let map (lines:seq<Line>) =
    let map = Array2D.create 1000 1000 0uy
    let increment (i,j) = map.[i,j]<-map.[i,j] + 1uy
    lines
    |> Seq.iter (range >> List.iter increment)
    map

let count src =
    [
        for i in 1 .. Array2D.length1 src
            -> src.[i-1, *] |> Array.filter ((<) 1uy) |> Array.length
    ] |> List.sum

[<Puzzle(2021, 5)>]
let puzzle case (input:seq<string>) =
    input
    |> Seq.map line
    |>
    match case with
    | Case.A -> Seq.filter nonDiagonal
    | Case.B -> id
    |> map
    |> count

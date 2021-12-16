module AdventOfCode.Cases.Y2021.Day15

open System
open System.IO
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

open AdventOfCode.Cases.Infrastructure.Parser
let parse: char[,]->int[,] = Array2D.map (fun v -> int v - 48)

let scale quantity (src:int[,])  =
    let size = Array2D.length1 src
    let init src i j =
        let targetVal = Array2D.get src <| i%size <| j%size
        (targetVal + (i / size) + (j / size) - 1) % 9 + 1
    Array2D.init <| size*quantity <| size * quantity <| init src

type State = | Risk of int | Path of int*int

let init src =
    let result = src |> Array2D.map Risk
    Array2D.set result 0 0 (Path (0,0))
    result

let adjacent src i j =
    let size1, size2 = Array2D.sizes src
    [
        if i = 0 then () else yield i - 1,j
        if i = size1 then () else yield i + 1,j
        if j = 0 then () else yield i,j - 1
        if j = size2 then () else yield i,j + 1
    ]
let update src path (i,j) =
    match Array2D.get src i j with
    | Path (v, r) -> Path (min v (path+r), r)
    | Risk r -> Path (path+r, r)
    |> Array2D.set src i j
let rec run src =
    let size, _ = Array2D.sizes src
    function
    | 0 -> src.[size, size]
    | step ->
        Array2D.iteri (
            fun i j v ->
                match v with
                | Path (v,r) ->
                    adjacent src i j
                    |> List.iter (update src v)
                | _ -> ()
        ) src
        run src (step - 1)

[<Puzzle(2021, 15)>]
let puzzle case (input:seq<string>) =
    let src =
        input
        |> array2D
        |> parse
        |> match case with
            | Case.A -> scale 1
            | Case.B -> scale 5

        |> init
    let size,_ = Array2D.sizes src
    match run src (size) with
    | Path (v,_) -> v
    | _ -> failwith "path not found"

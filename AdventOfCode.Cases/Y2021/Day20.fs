module AdventOfCode.Cases.Y2021.Day20

open System
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Image = int*int[,]

let pixel = function | '#' -> 1 | _ -> 0

let parseAlgorithm = Seq.map pixel >> Array.ofSeq
let parseImage = String.split Environment.NewLine >> Seq.map (Seq.map pixel) >> array2D

let parse = String.split $"{Environment.NewLine}{Environment.NewLine}"  >> function
    | [| algorithm; image |] ->
        let algorithm = parseAlgorithm algorithm
        algorithm, Image (0, parseImage image)
    | _ -> failwith "invalid input"

let getIndex fallBack src i j =
    let size1, size2 = Array2D.sizes src
    let adj = [for k in -1..1 do
                for l in -1..1 do
                    if i + k < 0 then yield fallBack
                    elif i + k > size1 then yield fallBack
                    elif j + l > size2 then yield fallBack
                    elif j + l < 0 then yield fallBack
                    else yield Array2D.get src (i + k) (j + l)
    ]
    List.reduce (fun acc cur -> acc * 2 + cur) adj

let initializer alg (fallback, src) i j =
        getIndex fallback src (i-1) (j-1)
        |> Array.get alg

let enhance algorithm (image:Image) =
    let size1, size2 = image |> snd |> Array2D.sizes
    let fallback = match fst image with | 1 -> Array.last algorithm | _-> Array.head algorithm
    let image = Array2D.init (size1 + 3) (size2 + 3) (initializer algorithm image)
    Image (fallback, image)

let run algorithm image steps =
    [1..steps] |> List.fold (fun acc _ -> enhance algorithm acc) image |> snd

let count = Array2D.collect (fun _ _ v -> v) >> List.sum

[<Puzzle(2021, 20)>]
let puzzle case (input:string) =
    input
    |> parse
    ||> run
    <| match case with
        | Case.A -> 2
        | Case.B -> 50
    |> count


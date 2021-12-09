module AdventOfCode.Cases.Y2021.Day9

open System.Drawing
open System.Dynamic
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections

let parse src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 0uy
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-byte v - 48uy ]] |> ignore
    map

let sizes map =
    let size1 = Array2D.length1 map - 1
    let size2 = Array2D.length2 map - 1
    size1, size2


let adjacent map (i,j) =
    let size1, size2 = sizes map
    [
        if i = 0 then () else yield i - 1,j
        if i = size1 then () else yield i + 1,j
        if j = 0 then () else yield i,j - 1
        if j = size2 then () else yield i,j + 1
    ]

let low map point = // all adjacent are greater
    let v = point ||> Array2D.get map
    adjacent map point
    |> List.map (fun (i,j) -> map.[i,j] > v)
    |> List.fold (&&) true
    |> function
        | true -> Some v
        | _ -> None

let mapLow mapper src =
    let size1, size2 = sizes src
    [for i in 0 .. size1 do
        for j in 0 .. size2 do
            match low src (i,j) with
            | Some v -> yield mapper (i,j) v
            | _ -> ()]

let caseA = mapLow (fun _ v -> v + 1uy)

let basin map point = // get adjacent grater than current but not 9 (max)
    let get = Array2D.get map
    let value = point ||> get
    let filter compare point  =
        let value = point ||> get
        value <> 9uy && value > compare
    let filter = filter value
    adjacent map point |> List.filter filter

let rec basins map acc = function
    | [] -> acc
    | points -> // for all get basin adjacent add them to result set (avoid duplicate)
        let points = List.collect (basin map) points
        let acc = points |> Set.ofList |> Set.union acc
        basins map acc points

let caseB src =
    let basins = basins src
    mapLow (fun p _ -> basins (Set [p]) [p]) src


[<Puzzle(2021, 9)>]
let puzzle case (input:seq<string>) =
    input
    |> parse
    |> match case with
        | Case.A -> caseA >> Seq.sumBy int64
        | Case.B ->
            caseB
            >> List.map Set.count
            >> List.sortDescending
            >> List.take 3
            >> List.map int64
            >> List.fold (*) 1L

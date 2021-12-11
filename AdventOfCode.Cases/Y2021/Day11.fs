module AdventOfCode.Cases.Y2021.Day11

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Dumbo = | Flash | Charge of int

let parse: char[,]->byte[,] = Array2D.map (fun v -> byte v - 48uy)

let adjacent src (i,j) =
    let size1, size2 = Array2D.sizes src
    [for k in -1..1 do
        for l in -1..1 do
            if i + k < 0 then ()
            elif i + k > size1 then ()
            elif j + l > size2 then ()
            elif j + l < 0 then ()
            elif k = 0 && l = 0 then ()
            else yield (i + k, j + l)
    ]

let flashLevel = 10uy

let flash src item  =
    let value = item ||> Array2D.get src |> (+) 1uy
    Array2D.set src <|| item <| value
    if value = flashLevel then Some item else None

let rec pulse flash (adjacent:int*int->list<int*int>) = flash >> function
    | Some v -> adjacent v
    | _ -> []

let rec pulseAll pulse items =
    match items with
    | [] -> ()
    | _ -> List.collect pulse items |> pulseAll pulse

let reset src  =
    Array2D.iteri (fun i j v -> if v >= flashLevel then Array2D.set src i j 0uy else()) src
    src

let step src =
    let pulseAll = pulse <| flash src <| adjacent src |> pulseAll
    src |> Array2D.alli |> pulseAll
    reset src |> Array2D.collect (fun _ _ v -> if v = 0uy then 1L else 0L ) |> List.sum

let rec steps src acc = function
    | 0 -> acc
    | cnt ->
        let acc = acc + step src
        steps src acc (cnt - 1)

let rec caseB src iteration =
    let allCount = (*) <| Array2D.length1 src <| Array2D.length2 src
    if step src >= allCount then iteration
        else caseB src (iteration + 1L)

[<Puzzle(2021, 11)>]
let puzzle case (input:seq<string>) =
    let input =
        input
        |> array2D
        |> parse
    match case with
        | Case.A -> steps input 0L 100
        | Case.B -> caseB input 1L

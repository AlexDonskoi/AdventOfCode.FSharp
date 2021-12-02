module AdventOfCode.Cases.Y2021.Day2

open AdventOfCode.Cases.Infrastructure

type Command =
    | Forward of int
    | Down of int
    | Up of int

let parse row =
    match String.split " " row with
    | [| "forward"; value |] -> value |> int |> Forward
    | [| "down"; value |] -> value |> int |> Down
    | [| "up"; value |] -> value |> int |> Up
    | c -> failwithf $"unknown command {c}"

let folderA (hor, depth) = function
    | Forward mv -> (hor + mv, depth)
    | Up mv -> (hor, depth - mv)
    | Down mv -> (hor, depth + mv)

let folderB (hor, depth, aim) = function
    | Forward mv -> (hor + mv, depth + mv*aim, aim)
    | Up mv -> (hor, depth, aim - mv)
    | Down mv -> (hor, depth, aim + mv)


[<Puzzle(2021, 2)>]
let puzzle case (input:seq<string>) =
    input
    |> Seq.map parse
    |> match case with
        | Case.A -> Seq.fold folderA (0, 0)
        | Case.B -> Seq.fold folderB (0, 0, 0) >> (fun (h, d, _) -> (h, d))
    ||> (*)

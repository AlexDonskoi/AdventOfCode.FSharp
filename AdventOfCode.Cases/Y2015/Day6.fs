module AdventOfCode.Cases.Y2015.Day6

open AdventOfCode.Cases.Utilities
open AdventOfCode.Cases.Infrastructure.Parser
open AdventOfCode.Cases.Infrastructure

type Command = | On | Off | Toggle

let private parsePoint = String.split "," >> function
    | [| Int x; Int y |] -> x,y
    | _ -> failwith "unable to parse segment"

let parse = String.split " " >> function
    | [| "turn"; "on"; st; "through"; fn |] -> On, (parsePoint st, parsePoint fn)
    | [| "turn"; "off"; st; "through"; fn |] -> Off, (parsePoint st, parsePoint fn)
    | [| "toggle"; st; "through"; fn |] -> Toggle, (parsePoint st, parsePoint fn)
    | _ -> failwith "unable to parse line"

let init = Array2D.create 1000 1000 0

let apply change arr (cmd, ((x1,y1), (x2,y2))) =
    [for i in x1 .. x2 do
         for j in y1 .. y2 do
             Array2D.get arr i j |> change cmd |> Array2D.set arr i j]
    |> ignore
    arr

let caseAModify = function
    | On -> fun _ -> 1
    | Off -> fun _ -> 0
    | Toggle -> function | 1 -> 0 | _ -> 1

let caseBModify = function
    | On -> fun v -> v + 1
    | Off -> fun v -> max 0 (v - 1)
    | Toggle -> fun v -> v + 2

[<Puzzle(2015, 6)>]
let puzzle case (input:seq<string>) =
    let modify =
        match case with
        | A -> caseAModify
        | B -> caseBModify

    let count acc = int64 >> (+) acc
    input
    |> Seq.map parse
    |> Seq.fold (apply modify) init
    |> Array2D.fold count 0
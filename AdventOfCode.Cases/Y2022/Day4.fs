module AdventOfCode.Cases.Y2022.Day4
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core


let intersectA ((fx, fy),(sx,sy)) =
    (fx <= sx && fy>=sy) || (fx >= sx && fy<=sy)

let intersectB ((fx, fy),(sx,sy)) =
    (fx <= sy && fy>=sx) || (fx >= sy && fy<=sx)

let parse src =
    match String.split "-" src with
    | [| Int x; Int y |] -> x, y
    | _ -> failwith "incorrect format"

let parseLine src =
    match String.split "," src with
    | [| f; s |] -> parse f, parse s
    | _ -> failwith "incorrect format"


[<Puzzle(2022, 4)>]
let puzzle case (source:seq<string>) =
    let intersect =
        match case with
        | Case.A -> intersectA
        | Case.B -> intersectB

    source
    |> Seq.map parseLine
    |> Seq.filter intersect
    |> Seq.length







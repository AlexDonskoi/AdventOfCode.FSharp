module AdventOfCode.Cases.Y2022.Day2
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let scoreA (src:string) =
    match src.Trim() with
    | "A X" -> 3 + 1
    | "A Y" -> 6 + 2
    | "A Z" -> 0 + 3
    | "B X" -> 0 + 1
    | "B Y" -> 3 + 2
    | "B Z" -> 6 + 3
    | "C X" -> 6 + 1
    | "C Y" -> 0 + 2
    | "C Z" -> 3 + 3
    | _ -> failwith "unknown value"

let scoreB (src:string) =
    match src.Trim() with
    | "A X" -> 0 + 3
    | "A Y" -> 3 + 1
    | "A Z" -> 6 + 2
    | "B X" -> 0 + 1
    | "B Y" -> 3 + 2
    | "B Z" -> 6 + 3
    | "C X" -> 0 + 2
    | "C Y" -> 3 + 3
    | "C Z" -> 6 + 1
    | _ -> failwith "unknown value"


[<Puzzle(2022, 2)>]
let puzzle case (source:seq<string>) =
    let score =
        match case with
        | Case.A -> scoreA
        | Case.B -> scoreB
    source
    |> Seq.map score
    |> Seq.sum







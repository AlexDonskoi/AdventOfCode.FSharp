module AdventOfCode.Cases.Y2022.Day6
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

[<Puzzle(2022, 6)>]
let puzzle case (source:string) =
    let window =
        match case with
        | Case.A -> 4
        | Case.B -> 14

    let condition = Seq.distinct >> Seq.length >> (=) window

    source
    |> Seq.windowed window
    |> Seq.tryFindIndex condition
    |> function
        | Some v -> v
        | None -> failwith "no match found"
    |> (+) window






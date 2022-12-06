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

    let condition (_, arr) =
        Seq.distinct arr |> Seq.length |> (=) 14

    source
    |> Seq.windowed window
    |> Seq.indexed
    |> Seq.tryFind condition
    |> Option.defaultValue (0, Array.empty<char>)
    |> fst
    |> (+) window






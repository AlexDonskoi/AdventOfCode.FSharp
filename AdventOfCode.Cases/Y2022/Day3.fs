module AdventOfCode.Cases.Y2022.Day3
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Core

let priority (src:char) =
    match int src with
    | a when a >= 97 -> a - 96
    | a -> a - 64 + 26

let compartment (src:string) =
    let take = Seq.length src / 2
    let first = Seq.take take src |> String.joinSeq ""
    let second = Seq.skip take src  |> String.joinSeq ""
    seq{first; second}

let groupA (src:seq<string>): seq<string> = Seq.collect compartment src

let groupB (src:seq<string>): seq<string> = Seq.map id src

let groupPriority (src:seq<string>) =
    src
    |> Seq.map Set.ofSeq
    |> Set.intersectMany
    |> Set.toSeq
    |> Seq.sumBy priority

[<Puzzle(2022, 3)>]
let puzzle case (source:seq<string>) =
    let grouping, size =
        match case with
        | Case.A -> groupA, 2
        | Case.B -> groupB, 3

    source
    |> grouping
    |> Seq.indexed
    |> Seq.groupBy (fun (i,e) -> i / size)
    |> Seq.map (snd >> Seq.map snd)
    |> Seq.map groupPriority
    |> Seq.sum







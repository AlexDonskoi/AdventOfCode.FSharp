module AdventOfCode.Cases.Y2021.Day25

open AdventOfCode.Cases.Infrastructure

[<Puzzle(2021, 1)>]
let puzzle case (input:seq<string>) =
    let size =
        match case with
        | Case.A -> 2
        | Case.B -> 4
    let folder acc cur =
        if Array.head cur < Array.last cur then acc + 1 else acc
    input
    |> Seq.map int
    |> Seq.windowed size
    |> Seq.fold folder 0

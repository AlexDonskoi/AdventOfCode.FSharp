module AdventOfCode.Cases.Y2021.Day6

open AdventOfCode.Cases.Infrastructure

let init src =
    let state = Array.create 9 0L
    let folder (acc:array<_>) cur =
        acc.[cur]<-acc.[cur] + 1L
        acc
    src
    |> String.split ","
    |> Array.map int
    |> Array.fold folder state

let rec next count (state:array<_>) =
    let cycle = function
        | 0 -> ignore
        | ind -> Array.set state (ind - 1)

    match count with
    | 0 -> state
    | _ ->
        let producers = Array.head state
        Array.iteri cycle state
        state.[6]<-state.[6] + producers
        next (count - 1) state

[<Puzzle(2021, 6)>]
let puzzle case (input:string) =
    input
    |> init
    |>
    match case with
    | Case.A -> next 80
    | Case.B -> next 256
    |> Array.sum

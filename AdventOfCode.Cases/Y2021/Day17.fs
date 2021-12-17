module AdventOfCode.Cases.Y2021.Day17

open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let rec withInY (mn, mx) velocity position step acc =
    match velocity + position with
    | pos when pos > mx ->
        withInY (mn, mx) (velocity + 1) pos (step + 1) acc
    | pos when pos <= mn ->
        let acc = List.append [step] acc
        withInY (mn, mx) (velocity + 1) pos (step + 1) acc
    | _ -> acc

let stepsY (mn, mx) =
    // mn is negative
    [mn..(-mn)] |> List.collect (fun v -> withInY (mn, mx) v 0 0 [])

type StepX = | Pass of int | Stay of int

let rec withInX (mn, mx) velocity position step acc =
    match velocity + position with
    | pos when pos < mn && velocity > 0  ->
        withInX (mn, mx) (max (velocity - 1) 0) pos (step + 1) acc
    | pos when pos >= mn && pos <= mx ->
        if velocity = 0 then Set.add (Stay step) acc
            else
                let acc = Set.add (Pass step) acc
                withInX (mn, mx) (max (velocity - 1) 0) pos (step + 1) acc
    | _ -> acc

let stepsX (mn, mx) =
    // mn is negative
    [0..mx] |> List.fold (fun acc v -> withInX (mn, mx) v 0 1 acc) Set.empty


let maxHeight (_, _) (ymn, _) = (ymn + 1)*ymn /2

let total (xmn, xmx) (ymn, ymx) =
    let rec reach (xmn, xmx) (ymn, ymx) (x,y) (vx, vy) =
        if x > xmx || y < ymn then false
        elif x < xmn || y > ymx then reach (xmn, xmx) (ymn, ymx) (x + vx, y + vy) (max (vx - 1) 0, vy - 1)
        else true
    let reach = reach (xmn, xmx) (ymn, ymx) (0,0)
    let folder reach acc cur = if reach cur then acc + 1 else acc
    [for vx in 1..xmx do for vy in ymn..(-ymn) do yield vx,vy]
    |> List.fold (folder reach) 0

let sourceRegex = Regex "^target area: x=(?<xmn>\d+)..(?<xmx>\d+), y=(?<ymn>-\d+)..(?<ymx>-\d+)"

let parse input =
    let getVal src name =
        match Regex.captures src name with
        | [Int v] -> v
        | _ -> failwith "incorrect source"
    let getVal = getVal <| sourceRegex.Match input
    (getVal "xmn", getVal "xmx"), (getVal "ymn", getVal "ymx")

let run = function
    | Case.A -> maxHeight
    | Case.B -> total

[<Puzzle(2021, 17)>]
let puzzle case (input:string) =
    input
    |> parse
    ||> run case


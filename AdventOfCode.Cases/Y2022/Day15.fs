module AdventOfCode.Cases.Y2022.Day15
open System.Collections.Generic
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let sourceRegex = Regex "Sensor at x=(?<sx>.+), y=(?<sy>.+): closest beacon is at x=(?<bx>.+), y=(?<by>.+)"

let parse input =
    let getVal src name =
        match Regex.captures src name with
        | [Int v] -> v
        | _ -> failwith "incorrect source"
    let getVal = sourceRegex.Match input |> getVal
    (getVal "sx", getVal "sy"), (getVal "bx", getVal "by")

let distance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let zone targetRow (sx, sy) distance =
     let adj = distance - abs (targetRow - sy)
     if adj >= 0 then Some (sx - adj, sx + adj) else None
     
               
[<Puzzle(2022, 15)>]
let puzzle case (source:seq<string>) =
    let source =
        source
        |> Seq.map parse
    let forbidden row =
        source
        |> Seq.map (fun (p1, p2) -> zone row p1 <| distance p1 p2)
        |> Seq.filter Option.isSome
        |> Seq.map (Option.defaultValue (0,0))
        |> Seq.toList
    let targetRow = 2000000
    let beacons = source |> Seq.map snd |> Set.ofSeq |> Seq.filter (fun (_, y) -> y = targetRow) |> Seq.length
    
    match case with
    | Case.A ->
        let forbidden = forbidden targetRow
        [for i in -2000000..20000000 -> i]
        |> Seq.sumBy (fun v -> if List.exists (fun (tx1, tx2) -> tx1 <= v && tx2 >= v) forbidden then 1 else 0)
        |> (-) <| beacons
        |> int64
    | Case.B ->
        let max = 4000000
        let source = Seq.map (fun (p1, p2) -> p1, distance p1 p2) source |> Seq.toList
        let allow cur (p, dist) = distance cur p > dist
        //let v = List.forall (allow (14,11)) source
        let rec check src = function
            | i,j when i >= max && j > max -> failwith "not found"
            | i,j when i > max -> check src (0, j + 1)
            | i,j when j > max -> check src (i + 1, 0)
            | i,j ->
                if List.forall (allow (i,j)) source then i, j
                else check src (i, j + 1)
        let (xr, yr) = check source (0, 0)
        let xr, yr = int64 xr, int64 yr
        yr + xr * 4000000L
        
                
module AdventOfCode.Cases.Y2023.Day18
open System
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let parseRow = String.split " " >> function
    | [| dir; Int mv; color |] -> dir, mv, color
    | _ -> failwith "wtf"
    
    
let rec move visited (i,j) = function
    | [] -> visited
    | (dir, mv)::rest ->
        let moves =
            match dir with
            | "R" -> [for k in 1..mv -> (i, j + k)]
            | "L" -> [for k in 1..mv -> (i, j - k)]
            | "U" -> [for k in 1..mv -> (i - k, j)]
            | "D" -> [for k in 1..mv -> (i + k, j)]
            | _ -> []
        let cur = List.last moves
        let visited = List.fold (fun acc c -> Set.add c acc) visited moves
        move visited cur rest

let rec wave digged si sj fi fj visited tmp =
    if Set.isEmpty tmp then visited else
        let cur = Set.minElement tmp
        let (ci, cj) = cur
        let tmp = Set.remove cur tmp
        let visited = Set.add cur visited
        let tmp =
            [
                if ci > si then (ci - 1), cj
                if ci < fi then (ci + 1), cj
                if cj > sj then ci, (cj - 1)
                if cj < fj then ci, cj + 1
            ]
            |> List.filter (fun c -> Set.contains c digged |> not)
            |> List.filter (fun c -> Set.contains c visited |> not)
            |> List.fold (fun acc c -> Set.add c acc) tmp 
        wave digged si sj fi fj visited tmp
    
let parseColor src =
    let src =
        src
        |> String.trimChar '('
        |> String.trimChar ')'
        |> String.trimChar '#'
    let dir =
        match Seq.last src with
        | '0' -> "R"
        | '1' -> "D"
        | '2' -> "L"
        | '3' -> "U"
        | _ -> failwith "NOOOOOOOO"
    let mv = Int32.Parse (Seq.take 5 src |> String.joinSeq "", NumberStyles.HexNumber)
    dir, mv
 
let run source =
    let digged = move <| Set.singleton (0,0) <| (0, 0) <| source
    let rows = Seq.map fst digged
    let cols = Seq.map snd digged
    let si, sj = (Seq.min rows), (Seq.min cols)
    let fi, fj = (Seq.max rows), (Seq.max cols)
    
    let around = wave digged (si - 1) (sj - 1) (fi + 1) (fj + 1) Set.empty <| Set.singleton (si - 1, sj - 1)
    
    let total = (fi - si + 3) * (fj - sj + 3)
    (-) total <| Set.count around   
    

[<Puzzle(2023, 18)>]
let puzzle case (source:seq<string>) =
    source |> Seq.map parseRow |> Seq.toList
    |>  match case with
        | Case.A -> List.map (fun (a,b,_) -> (a,b))
        | Case.B -> List.map (fun (_,_,c) -> parseColor c)
    |> run
    
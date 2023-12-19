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
 
 
let rec collectLines source rows cols cur =
    match source with
    | [] -> rows, cols
    | h::rest ->
        let i,j = cur
        let move = snd h
        let (fi, fj) =
            match fst h with
            | "R" -> (i, j + move)
            | "D" -> (i + move, j)
            | "L" -> (i, j - move)
            | "U" -> (i - move, j)
            | _ -> failwith "NOOOOOOOO"
        let rows = if fi = i then (i, (min j fj, max j fj))::rows else rows   
        let cols = if fj = j then (j, (min i fi, max i fi))::cols else cols   
        collectLines rest rows cols (fi, fj)   

let countInside rows cols (si, sj) (fi, fj) =
    let crossRows = List.fold (fun acc (i, (csj, cfj)) -> if si <= i && fi > i && csj <= fj && cfj >= sj then acc + 1 else acc) 0 rows    
    let crossCols = List.fold (fun acc (j, (csi, cfi)) -> if sj <= j && fj > j && csi <= fi && cfi >= si then acc + 1 else acc) 0 cols
    if crossRows % 2 = 1 && crossCols  % 2 = 1 then (fj-sj |> int64 |> (+) 1L)*(fi-si |> int64 |> (+) 1L) else 0L
 
let gaps points =
    let points =
        points
        |> List.map fst
        |> List.collect (fun i -> [i-1;i;i+1])
        |> List.sort
    let points = 
    let mx = Set.maxElement points
        
    let p1, p2 = points |> Set.remove mn |> Set.remove mx |> Set.toList |> List.indexed |> List.partition (fun v -> fst v |> (%) <| 2 = 0)
    let p1 = List.map fst p1
    let p2 = List.map fst p2
    List.map2 (fun a b -> a,b) p1 p2   
    
 
let run source =
    let rows, cols = collectLines source List.empty List.empty (0, 0)
    let rowPairs = gaps rows
    let colPairs = gaps cols
    List.allPairs rowPairs colPairs
    |> List.sumBy (fun ((si, fi), (sj, fj)) -> countInside rows cols (si, sj) (fi, fj))
    

[<Puzzle(2023, 18)>]
let puzzle case (source:seq<string>) =
    source |> Seq.map parseRow |> Seq.toList
    |>  match case with
        | Case.A -> List.map (fun (a,b,_) -> (a,b))
        | Case.B -> List.map (fun (_,_,c) -> parseColor c)
    |> run
    
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
        let row, col, fi, fj =
            match fst h with
            | "R" -> Some (i, (j, (j + move))), None, i, j + move
            | "D" -> None, Some (j, (i, (i + move))), i + move, j
            | "L" -> Some (i, ((j - move), j)), None, i, j - move
            | "U" -> None, Some (j, (i - move, i)), i - move, j
            | _ -> failwith "NOOOOOOOO"
        
        let rows = match row with | Some r -> r::rows | _ -> rows
        let cols = match col with | Some c -> c::cols | _ -> cols
        collectLines rest rows cols (fi, fj)   

let countInside rows cols (si, sj) (fi, fj) =
    
    
    //let catchBorder = List.filter (fun (ri, (rsj, rfj)) -> si <= ri && ri <= fi && fj >= rsj && sj <= rfj ) rows |> List.length
    //let catchBorder = List.filter (fun (cj, (csi, cfi)) -> si <= cfi && fi >= csi && fj >= cj && sj <= cj ) cols  |> List.length |> (+) catchBorder
    
    //if catchBorder then 0L else
    let crossRows = List.fold (fun acc (ri, (csj, cfj)) -> if si > ri  && csj <= fj && cfj >= sj then acc + 1 else acc) 0 rows    
    let crossCols = List.fold (fun acc (cj, (rsi, rfi)) -> if cj < sj && rsi <= fi && rfi >= si then acc + 1 else acc) 0 cols
    if (crossRows + crossCols) % 2 = 1 then
        let cnt = (fj-sj |> int64 |> (+) 1L)*(fi-si |> int64 |> (+) 1L)
        printf $"{si}-{sj}, {fi}-{fj} "
        printfn $"%d{cnt}"
        cnt
        else 0L
    
 
let gaps points =
    let points = points |> List.map fst |> List.sort
    let head = List.head points
    points    
    |> List.pairwise
    |> List.collect (fun (a,b) -> [(a+1,b-1); (b,b)])
    |> List.append [head, head]
    |> List.filter (fun c -> c ||> (<=))
    |> List.distinct
 
let run source =
    let rows, cols = collectLines source List.empty List.empty (1, 1)
    let rowPairs = gaps rows
    let colPairs = gaps cols
    let cnt =
        List.allPairs rowPairs colPairs
        |> List.sumBy (fun ((si, fi), (sj, fj)) -> countInside rows cols (si, sj) (fi, fj))
    cnt
    |> (+) <| (List.sumBy (fun (_, (s,f)) -> f - s) rows |> int64)
    |> (+) <| (List.sumBy (fun (_, (s,f)) -> f - s) cols |> int64)

[<Puzzle(2023, 18)>]
let puzzle case (source:seq<string>) =
    let source =
        source |> Seq.map parseRow |> Seq.toList
        |>  match case with
            | Case.A -> List.map (fun (a,b,_) -> (a,b))
            | Case.B -> List.map (fun (_,_,c) -> parseColor c)
    source |> run
    
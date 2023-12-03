module AdventOfCode.Cases.Y2023.Day2
open System
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let parseId input =
    match String.split " " input with
    | [| "Game"; Int game |] -> game
    | _ -> failwith $"unexpected source {input}"
    
let parseCube input =
    match String.split " " input with
    | [| Int num; color |] -> color, num
    | _ -> failwith $"unexpected source {input}"   
 
let parseCubes input =
    String.split ", " input
    |> Seq.map parseCube
    |> Map.ofSeq 
  
let parseGames input =
    String.split "; " input
    |> Seq.map parseCubes  
  
let parseRow input =
    match String.split ": " input with
    | [| game; cubes |] ->
        (parseId game, parseGames cubes)
    | _ -> failwith $"unexpected source {input}"
    
let validA (r, g, b) map =
    (map |> Map.exists (fun k v -> k = "red" && v > r) |> not)
    && (map |> Map.exists (fun k v -> k = "green" && v > g) |> not)
    && (map |> Map.exists (fun k v -> k = "blue" && v > b) |> not)
    
let rowResultA (num, maps) =
    if Seq.forall (validA (12, 13, 14)) maps then num else 0
    
let rowResultB (num, maps) =
    let folder map k v = Map.change k (fun cur -> Option.defaultValue 0 cur |> max v |> Some) map
    let folder = Map.fold folder
    let required = Seq.fold folder Map.empty maps
    ["red"; "green"; "blue"]
    |> List.map (fun k -> Map.tryFind k required |> Option.defaultValue 0)
    |> List.fold (*) 1

[<Puzzle(2023, 2)>]
let puzzle case (source:seq<string>) =
    let result =
        parseRow
        >> match case with
            | Case.A -> rowResultA
            | Case.B -> rowResultB
            
    source
    |> Seq.map result
    |> Seq.sum 







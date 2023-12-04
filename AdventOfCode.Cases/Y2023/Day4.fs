module AdventOfCode.Cases.Y2023.Day4
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let parseRow input =
    let captures = Regex.Match(input, $"Card +(?<card>\d+): *(?:(?<win>\d+) *)+\|(?: *(?<num>\d+))+") |> Regex.captures
    let game =
        captures "card"
        |> function
            | [Int v] -> v
            | v -> failwith $"incorrect source {v}"
    let wins = captures "win" |> List.map int
    let nums = captures "num" |> List.map int
    
    let points =
        Set.intersect
        <| Set.ofList wins
        <| Set.ofList nums
        |> Set.count
    game, points
    
let folderA acc (game, points) =
    points - 1
    |> pown 2
    |> (+) acc
  
let rec addDuplicates map num =
    function
        | [] -> map
        | h::rest ->
            map
            |> Map.change h (Option.defaultValue 0 >> (+) num >> Some)
            |> addDuplicates
            <| num
            <| rest            
  
let rec duplicates map = function
    | [] -> map
    | (g, p)::rest ->
        let map = Map.change g (Option.defaultValue 0 >> (+) 1 >> Some) map
        let quantity = map |> Map.find g
        [for i in g+1 .. g+p -> i]
        |> addDuplicates map quantity
        |> duplicates
        <| rest

[<Puzzle(2023, 4)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.map parseRow |> Seq.toList
    match case with
    | Case.A ->
        Seq.fold folderA 0 source
    | Case.B ->
        source
        |> duplicates Map.empty
        |> Map.values
        |> Seq.sum





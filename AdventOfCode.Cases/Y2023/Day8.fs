module AdventOfCode.Cases.Y2023.Day8
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

  
let parseRow input =
    let captures = Regex.Match(input, $"(?<src>\w+) = \((?<left>\w+), (?<right>\w+)\)") |> Regex.captures >> List.head
    let src = captures "src"
    let left = captures "left"
    let right = captures "right"
    src, (left, right)
 
let rec moves maps initMove curMove pos step =
    match curMove with
    | [] -> moves maps initMove initMove pos step
    | h::rest ->
        let choose = if h = 'L' then fst else snd 
        let nextPos = maps |> Map.find pos |> choose
        let step = step + 1
        if nextPos = "ZZZ" then step else
            moves maps initMove rest nextPos step
    
[<Puzzle(2023, 8)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.toList
    let movesMap = source |> List.head |> Seq.toList
    let maps = source |> List.skip 1 |> List.map parseRow |> Map.ofList
    
    match case with
    | Case.A -> moves maps movesMap movesMap "AAA" 0                      
    | Case.B -> 0
    
    
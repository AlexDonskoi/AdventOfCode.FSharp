module AdventOfCode.Cases.Y2023.Day2
open System
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let capture =
        function
        | ["one"] -> 1
        | ["two"] -> 2
        | ["three"] -> 3
        | ["four"] -> 4
        | ["five"] -> 5
        | ["six"] -> 6
        | ["seven"] -> 7
        | ["eight"] -> 8
        | ["nine"] -> 9
        | [Int v] -> v
        | _ -> failwith "incorrect source"   
    
let parsePattern casePattern input =
    let num1 = Regex.Match(input, $"[^{casePattern}]*(?<num>{casePattern}).*") |> Regex.captures <| "num" |> capture        
    let num2 = Regex.Match(input, $".*(?<num>{casePattern})[^{casePattern}]*") |> Regex.captures <| "num" |> capture
        
    num1 * 10 + num2

let parseA = parsePattern "\d"
let parseB = parsePattern "(\d|one|two|three|four|five|six|seven|eight|nine)"

[<Puzzle(2023, 2)>]
let puzzle case (source:seq<string>) =
    let parse =
        match case with
        | Case.A -> parseA
        | Case.B -> parseB
    source
    |> Seq.map parse
    |> Seq.sum







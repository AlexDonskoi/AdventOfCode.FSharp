module AdventOfCode.Cases.Y2023.Day6
open System
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let parseRow input = [for m in Regex.Matches(input, "\d+") -> int64 m.Value ]
    
let rec count dist tm hold =
    if (tm - hold) * hold > dist then (tm - hold) - hold + 1L
    else if hold >= tm then 0L
    else count dist tm <| hold + 1L

[<Puzzle(2023, 6)>]
let puzzle case (source:seq<string>) =
    let mapper a b = count a b 1
    let transform =
        match case with
        | Case.A -> id
        | Case.B -> String.replace " " ""
        >> parseRow
    let source= source|> Seq.map transform
    source 
    |> Seq.skip 1
    |> Seq.head
    |> List.map2 mapper
    <| Seq.head source
    |> Seq.fold (*) 1L
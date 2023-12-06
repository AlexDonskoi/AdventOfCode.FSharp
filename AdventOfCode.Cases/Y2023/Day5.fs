module AdventOfCode.Cases.Y2023.Day5
open System
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let parseLine = String.split " " >> Seq.map int64 >> Seq.toList
   
let parseSource =
    String.split ": " >> 
    function
    | [|_; ids|] -> parseLine ids
    | s -> failwith $"incorrect source {s}"

let parseMap =
    String.split $":{Environment.NewLine}" >>
    function
    | [|_; lines|] -> lines |> String.split Environment.NewLine |> Seq.map parseLine |> Seq.toList
    | s -> failwith $"incorrect source {s}"
  
let isMapped map value =
    [for dest::src::len::_ in map do
        let diff = value - dest
        if diff >= 0L && diff < len then yield src + diff
            else ()]
let isPassed map value =
    let search v row =
        let _::src::len::_ = row
        let diff = v - src
        diff >= 0L && diff < len
    List.exists (search value) map |> not

let rec steps maps values =
    match maps with
    | [] -> values
    | h::rest ->
        let values =
             [for v in values do
                 yield! isMapped h v
                 if isPassed h v then yield v else()
                 ]                    
        steps rest values

let rec toPairs acc = function
    | [] -> acc
    | f::s::rest-> toPairs <| (f, f + s)::acc <| rest
    | _ -> failwith "wtf"

let rec check seeds = function
    | [] -> false
    | h::rest ->
        if List.exists(fun (s,f) -> s <= h && h <=f) seeds then true
            else check seeds rest
    
let rec search check maps num =
    let src = maps [num]
    if check src then num else search check maps <| num + 1L

[<Puzzle(2023, 5)>]
let puzzle case (source:string) =
    let source, maps = 
        source
        |> String.split $"{Environment.NewLine}{Environment.NewLine}"
        |> Array.toList
        |> function
            | [] -> failwith "incorrect source"
            | h::res -> h, res
            
    let steps = List.map parseMap maps |> List.rev |> steps
    let source =
        parseSource source
        |> match case with
            | Case.A -> List.map (fun a -> (a, a))
            | Case.B -> toPairs []
            
    search <| check source <| steps <| 0L
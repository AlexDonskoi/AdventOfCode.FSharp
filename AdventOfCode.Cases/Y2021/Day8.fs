module AdventOfCode.Cases.Y2021.Day8

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections

let parse =
    let normalize = Seq.sort >> String.joinSeq ""
    let normalize = String.split " " >> Array.map normalize >> Array.toList
    String.split "|" >>
    function
    | [| patterns; output |] -> (normalize patterns, normalize output)
    | _ -> failwith "incorrect input"

let caseA src =
    let filter = Seq.length >> function |2 |4 |3 |7 -> true | _ -> false
    let count = List.filter filter >> List.length

    src
    |> Seq.map (snd >> count)
    |> Seq.sum

let map (src:list<string>) =
    let join = String.joinSeq ""
    let intersect fst snd = Set.intersect <| Set.ofSeq fst <| Set.ofSeq snd |> join
    let byLength len = Map.find len
    let intersectsBy len fst =
        intersect fst >> Seq.length >> (=) len |> List.filter
    let exclude value = List.filter ((<>) value)

    let map = src |> List.groupBy Seq.length |> Map

    let digit1 = byLength 2 map |> Seq.head
    let digit4 = byLength 4 map |> Seq.head
    let digit7 = byLength 3 map |> Seq.head
    let digit8 = byLength 7 map |> Seq.head

    let digit069 = byLength 6 map
    let digit9 = digit069 |> intersectsBy 4 digit4 |> Seq.head
    let digit06 = digit069 |> exclude digit9
    let digit0 = digit06 |> intersectsBy 2 digit1 |> Seq.head
    let digit6 = digit06 |> exclude digit0 |> Seq.head

    let digit5 = intersect digit6 digit9
    let digit23 = byLength 5 map |> exclude digit5
    let digit3 = digit23 |> intersectsBy 2 digit1 |> Seq.head
    let digit2 = digit23 |> exclude digit3 |> Seq.head

    Map [(digit0, 0); (digit1, 1); (digit2, 2); (digit3, 3); (digit4, 4); (digit5, 5); (digit6, 6); (digit7, 7); (digit8, 8); (digit9, 9) ]

let rec output num map = function
    | [] -> num
    | h::rest ->
        let num = Map.find h map + num * 10
        output num map rest

let caseB =
    let mapper (pattern, src:list<string>) =
        output 0 <| map pattern <| src
    Seq.sumBy mapper

[<Puzzle(2021, 8)>]
let puzzle case (input:seq<string>) =
    input
    |> Seq.map parse
    |> match case with
        | Case.A -> caseA
        | Case.B -> caseB

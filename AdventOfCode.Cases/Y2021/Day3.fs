module AdventOfCode.Cases.Y2021.Day3

open AdventOfCode.Cases.Infrastructure

let parse = Seq.map int >> Seq.map ((-) 48 >> (*) -1) >> Seq.toList

let toNumber =
    List.map int64 >> List.fold (fun acc cur -> acc * 2L + cur) 0L

let split =
    let oxySelector (f, s) = if List.length f < List.length s then s else f
    let co2Selector (f, s) = if List.length f < List.length s then f else s
    function
    | []::_ -> ([], [])
    | src -> src
             |> List.partition (List.head >> (=) 1)
             |> (fun pair -> (oxySelector pair, co2Selector pair))

let rec rate select acc = function
    | [] -> acc
    | [ h ] -> List.fold (fun acc cur -> acc * 2L + (int64 cur)) acc h
    | []::_ -> acc
    | src ->
        let adj = src |> List.head |> List.head |> int64
        let acc = acc * 2L + adj
        let src = src |> List.map List.tail |> split |> select
        rate select acc src

let caseB src =
    let (oxy, co2) = split src
    let oxy = rate fst 0L oxy
    let co2 = rate snd 0L co2
    oxy, co2

let caseA src =
    let folder (g, e) cur =
        let (g, e) = (g * 2L, e * 2L)
        if cur >= 500
            then (g + 1L, e)
            else (g, e + 1L)
    List.fold
    <| List.map2 (+)
    <| List.head src
    <| List.tail src
    |> List.fold folder (0L, 0L)


[<Puzzle(2021, 3)>]
let puzzle case (input:seq<string>) =
    input
    |> Seq.map parse
    |> Seq.toList
    |> match case with
        | Case.A -> caseA
        | Case.B -> caseB
    ||> (*)
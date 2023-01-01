module AdventOfCode.Cases.Y2022.Day25

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Core


let snafuMultiplier = function
    | '2' -> 2L
    | '1' -> 1L
    | '0' -> 0L
    | '-' -> -1L
    | '=' -> -2L
    | _ -> failwith "wtf"

let rec decRec source acc =
    match Seq.tryHead source with
    | Some v ->
        let acc = v |> snafuMultiplier |> List.singleton |> List.append acc
        decRec <| Seq.tail source <| acc
    | None ->
        List.reduce (fun f s ->  f * 5L + s) acc

let dec source = decRec source List.Empty

let sumChar f s =
    match f, s with
    | '=', '=' -> '1', Some '-'
    | '=', '-' | '-', '=' -> '2', Some '-'
    | '=', '1' | '1', '=' -> '-', None
    | '=', '2' | '2', '=' -> '0', None
    | '-', '-'  -> '=', None
    | '-', '1' | '1', '-'  -> '0', None
    | '-', '2' | '2', '-'  -> '1', None
    | '1', '2' | '2', '1'  -> '=', Some '1'
    | '1', '1' -> '2', None
    | '2', '2'   -> '-', Some '1'
    | '0', _ -> s, None
    | _, _ -> f, None

let rec sumSingle num ch acc =
    match num with
    | [] -> acc
    | h::rest ->
        match sumChar h ch with
        | v, None -> List.append acc (v::rest)
        | v, Some a ->
            let acc = List.append acc [v]
            sumSingle rest a acc

let rec sumRec f s acc=
    match f, s with
    | [], _ -> List.append acc s
    | _, [] -> List.append acc f
    | _, h::rest ->
        let f = sumSingle f h []
        let acc = List.append acc <| List.take 1 f
        sumRec <| List.skip 1 f <| rest <| acc
let sum f s = sumRec f s []

let parse = Seq.rev >> Seq.toList

[<Puzzle(2022, 25)>]
let puzzle case (source:seq<string>) =

    match case with
    | Case.A ->
        source
        |> Seq.map parse
        |> Seq.reduce sum
        |> Seq.rev
        |> String.joinSeq ""
    | Case.B ->
        "done!!"
module AdventOfCode.Cases.Y2021.Day16

open System
open System.IO
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core


type Packet = | Literal of int*int*int64 | Operator of int*int*list<Packet> | Empty

let toInt (src:seq<byte>) = Seq.fold (fun acc cur ->int cur + acc*2 ) 0 src

let extract len src =
    if Seq.length src < len then Seq.empty, Seq.empty
        else Seq.take len src, Seq.skip len src

let rec literals source =
    let next, rest = extract 5 source
    let value = Seq.tail next |> toInt
    [
        yield value, rest
        if Seq.head next = 1uy then yield! literals rest else ()
    ]

let rec all next source =
    [
        match Seq.tryHead source with
        | Some _ ->
            let packet, rest = next source
            match packet with
            | Empty -> ()
            | _ ->
                yield packet
                yield! all next rest
        | _ -> ()
    ]

let rec packet source =
    let version, rest = extract 3 source
    let version = toInt version
    let typeId, rest = extract 3 rest
    let typeId = toInt typeId
    match typeId with
    | 4 ->
        let literals = literals rest
        let rest = List.last literals |> snd
        let literal = List.map fst literals |>  List.fold (fun acc cur -> int64 cur + acc*16L) 0L
        //let skip = 4 - (6 + (List.length literals)*5 )%4
        //let _, rest = extract skip rest
        Literal (version, typeId, literal), rest
    | _ ->
        let flag, rest = extract 1 rest
        match toInt flag with
        | 0 ->
            let len, rest = extract 15 rest
            match toInt len with
            | 0 -> Empty, Seq.empty
            | len ->
                let subPackages, rest = extract len rest
                Operator (version, typeId, all packet subPackages), rest
        | _ ->
            let count, rest = extract 11 rest
            match toInt count with
            | 0 -> Empty, Seq.empty
            | count ->
                let folder (acc,rest) _ =
                    let it, rest = packet rest
                    List.append acc [it],rest
                let inner, rest = [1..count] |> List.fold folder ([], rest)
                Operator (version, typeId, inner), rest

let bits = function
    | '0' -> [0uy; 0uy; 0uy; 0uy]
    | '1' -> [0uy; 0uy; 0uy; 1uy]
    | '2' -> [0uy; 0uy; 1uy; 0uy]
    | '3' -> [0uy; 0uy; 1uy; 1uy]
    | '4' -> [0uy; 1uy; 0uy; 0uy]
    | '5' -> [0uy; 1uy; 0uy; 1uy]
    | '6' -> [0uy; 1uy; 1uy; 0uy]
    | '7' -> [0uy; 1uy; 1uy; 1uy]
    | '8' -> [1uy; 0uy; 0uy; 0uy]
    | '9' -> [1uy; 0uy; 0uy; 1uy]
    | 'A' -> [1uy; 0uy; 1uy; 0uy]
    | 'B' -> [1uy; 0uy; 1uy; 1uy]
    | 'C' -> [1uy; 1uy; 0uy; 0uy]
    | 'D' -> [1uy; 1uy; 0uy; 1uy]
    | 'E' -> [1uy; 1uy; 1uy; 0uy]
    | 'F' -> [1uy; 1uy; 1uy; 1uy]
    | _ -> []

let rec versions src =
    List.collect (function | Literal (v,_,_) -> [v] | Operator (v,_,nested) -> versions nested |> List.append [v] | _ -> []) src

let rec result = function
    | Literal (_, _, v) -> v
    | Operator (_, 0, nested) ->
        List.map result nested |> List.sum
    | Operator (_, 1, nested) ->
        List.map result nested |> List.reduce (*)
    | Operator (_, 2, nested) ->
        List.map result nested |> List.min
    | Operator (_, 3, nested) ->
        List.map result nested |> List.max
    | Operator (_, 5, nested) ->
        let f::s::_ = List.map result nested
        if f > s then 1 else 0
    | Operator (_, 6, nested) ->
        let f::s::_ = List.map result nested
        if f < s then 1 else 0
    | Operator (_, 7, nested) ->
        let f::s::_ = List.map result nested
        if f = s then 1 else 0
    | _ -> failwith "unknown type id"



let parse = Seq.collect bits >> all packet

[<Puzzle(2021, 16)>]
let puzzle case (input:string) =
    input
    |> parse
    |> match case with
        | Case.A -> versions >> List.sum >> int64
        | Case.B -> List.head >> result


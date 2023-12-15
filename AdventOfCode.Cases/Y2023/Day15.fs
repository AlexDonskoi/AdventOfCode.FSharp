module AdventOfCode.Cases.Y2023.Day15
open System
open System.Diagnostics
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let parseInput = String.replace Environment.NewLine "" >> String.split "," >> Array.toList

let rec hash acc src =
    match Seq.tryHead src with
    | None -> acc
    | Some h ->
        let value =
            int h |> (+) acc
            |> (*) 17
            |> (%) <| 256
        hash value <| Seq.tail src

type Command = | Set of string*int | Remove of string

let parseCmd src =
    match String.split "=" src with
    | [| k; Int len |] -> Set (k,len)
    | _ -> src |> String.replace "-" "" |> Remove

let removeFromBox k = function
    | None -> None
    | Some list ->
        match List.tryFindIndex (fst >> (=) k) list with
        | None -> Some list
        | Some i ->
            let list = List.removeAt i list
            if List.isEmpty list then None else Some list

let replaceInBox (k,v) = function
    | None -> Some [k,v]
    | Some list ->
        match List.tryFindIndex (fst >> (=) k) list with
        | None -> List.append list [k,v] |> Some
        | Some i ->
            let list = List.updateAt i (k,v) list
            Some list        

let rec processBoxes map = function
    | [] -> map
    | h::rest ->
        match parseCmd h with
        | Remove k ->
            let box = hash 0 k 
            let map = Map.change box (removeFromBox k) map
            processBoxes map rest
        | Set (k,v) ->
            let box = hash 0 k 
            let map = Map.change box (replaceInBox (k,v)) map
            processBoxes map rest
        

[<Puzzle(2023, 15)>]
let puzzle case (source:string) =
    let source = source |> parseInput
    match case with
    | Case.A -> List.fold (fun acc cur -> hash 0 cur |> (+) acc) 0 source
    | Case.B ->
        let folder acc k v =
            List.indexed v
            |> List.fold (fun acc (i, (_, l)) -> (k + 1) * (i + 1)* l + acc) 0
            |> (+) acc
        source
        |> processBoxes Map.empty
        |> Map.fold folder 0 
        
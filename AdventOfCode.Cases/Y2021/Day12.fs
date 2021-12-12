module AdventOfCode.Cases.Y2021.Day12

open System.Collections.Generic
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

let parseLine = String.split "-" >> function
    | [| f; s |] -> f,s
    | _ -> failwith "incorrect path"

let addRoute start finish =
    match start, finish with
    | _, "start" -> id
    | "end", _ -> id
    | s,f ->
        Map.change start
        <| function
            | Some caves -> finish::caves |> Some
            | _ -> Some [finish]

let add (f,s) = addRoute f s >> addRoute s f

let map = Seq.fold (fun acc cur -> add cur acc) Map.empty

let finish = "end"
let start = "start"

let continuations isValid map path =
    let last = List.head path
    let isValid = isValid path
    let connections = Map.find last map
    List.filter isValid connections
    |> List.map (fun it -> it::path)

let rec allPaths isValid map paths =
    let continuation = continuations isValid map
    let completed, rest = List.partition (List.head >> (=) finish) paths
    match List.collect continuation rest with
    | [] -> completed
    | paths -> allPaths isValid map <| List.append paths completed

let isValidA path cur =
    let last = List.head path
    if List.pairwise path |> List.contains (cur,last)
        then false
        elif (cur <> String.upper cur) && List.contains cur path then false
        else true

let isValidB path cur =
    let last = List.head path
    let smallVisitedTwice =
        path
        |> List.filter (fun it -> it <> String.upper it)
        |> List.countBy id
        |> List.filter (snd >> (<) 1)
        |> List.tryHead
        |> Option.isSome
    let isBig = cur = String.upper cur
    if List.contains finish path then false
    elif isBig then true
    elif cur = start then false
    elif smallVisitedTwice then List.contains cur path |> not
    else true


[<Puzzle(2021, 12)>]
let puzzle case (input:seq<string>) =
    let map = input |> Seq.map parseLine |> map
    let start = List.singleton [start]
    allPaths
    <| match case with
        | Case.A -> isValidA
        | Case.B -> isValidB
    <| map
    <| start
    |> List.length

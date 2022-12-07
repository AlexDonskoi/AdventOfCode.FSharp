module AdventOfCode.Cases.Y2022.Day7
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

type Content = | File of string * int | Directory of string



let parseContent (src:string)=
    match String.split " " src with
    | [| "dir"; dir |] -> Directory dir
    | [| Int size; name |] -> File (name,size)
    | _ -> failwith "unrecognized format"

let join = List.fold (fun acc cur -> acc + string cur) ""



let updateTree (dir:list<string>) size =
    let path = List.rev dir |> List.reduce (+)
    Map.change path (Option.defaultValue 0 >> (+) size >> Some)

let rec updateTreeRec dir size tree =
    match dir with
    | "/"::_::rest ->
        let tree = updateTree dir size tree
        updateTreeRec rest size tree
    | _ -> updateTree ["/"] size tree


let treeA (path, tree) = Seq.toList >> function
    | ['$'; ' '; 'c'; 'd'; '/'] -> ["/"], tree
    | ['$'; ' '; 'c'; 'd'; ' '; '.' ; '.'] -> List.skip 2 path, tree
    | '$'::' '::'c'::'d'::dir ->
        let dir = String.joinSeq "" dir
        "/"::dir::path, tree
    | ['$';' ';'l'; 's'] -> path, tree
    | 'd'::'i'::'r'::dir -> path, tree
    | file ->
        match file |> join |> String.split " " with
        | [| Int size; _ |] ->
            let tree = updateTreeRec path size tree
            path, tree
        | _ -> failwith "no file size"
    | _ -> failwith "unexpected input"

[<Puzzle(2022, 7)>]
let puzzle case (source:seq<string>) =
    let map =
        source
        |> Seq.fold treeA (["/"], Map.empty)
        |> snd
    match case with
        | Case.A ->
            map
            |> Map.filter (fun k v -> v <= 100000)
            |>Map.fold (fun acc k v -> acc + v) 0
        | Case.B ->
            let space = Map.find "/" map - 40000000
            map
            |> Map.toList
            |> List.map snd
            |> List.filter ((<=) space)
            |> List.min






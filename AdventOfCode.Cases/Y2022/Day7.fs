module AdventOfCode.Cases.Y2022.Day7
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
open Microsoft.Win32.SafeHandles

type Command =
    | Root
    | MoveUp
    | Move of string
    | Ls
    | File of string * int
    | Directory of string

type Path = list<string>

let path = List.rev >> String.concat "/"

let parseCommand src =
    match String.split " " src with
    | [| "$"; "cd"; "/" |] -> Root
    | [| "$"; "cd"; ".." |] -> MoveUp
    | [| "$"; "cd"; dir |] -> Move dir
    | [| "$"; "ls" |] -> Ls
    | [| "dir"; dir |] -> Directory dir
    | [| Int size; name |] -> File (name,size)
    | _ -> failwith "unrecognized command"

let increaseSize key size = Map.change key (Option.defaultValue 0 >> (+) size >> Some)
let rec increaseSizeRecursive key size tree =
    match key with
    | ["/"] -> "/" |> increaseSize <| size <| tree
    | _::rest ->
        let tree = key |> path |> increaseSize <| size <| tree
        increaseSizeRecursive rest size tree
    | _ -> tree

let foldTree (path, tree) cmd =
    match cmd with
    | Root -> ["/"], tree
    | MoveUp -> List.tail path, tree
    | Move dir -> dir::path, tree
    | File (_,size)->
        path, increaseSizeRecursive path size tree
    | _ -> path, tree

let run =
    Seq.map parseCommand
    >> Seq.fold foldTree (["/"], Map.empty)
    >> snd

let updateTree (dir:list<string>) size =
    let path = List.rev dir |> List.reduce (+)
    Map.change path (Option.defaultValue 0 >> (+) size >> Some)

let folderA acc _ v = if v <= 100000 then acc + v else acc

let folderB (limit, acc) _ v =
    let isMatch = (v >= limit) && (v < acc)
    limit, if isMatch then v else acc

[<Puzzle(2022, 7)>]
let puzzle case (source:seq<string>) =
    let map = run source
    match case with
    | Case.A -> Map.fold folderA 0 map
    | Case.B ->
        let space = "/" |> Map.find <| map |> (-) <| 40000000
        Map.fold folderB (space, 70000000) map
        |> snd






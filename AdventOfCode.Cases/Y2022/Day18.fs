module AdventOfCode.Cases.Y2022.Day18
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let parseCube src =
    match String.split "," src with
    | [| Int x; Int y; Int z |] -> x, y, z
    | _ -> failwith "unknown format"

let neighbours cur =
    let x,y,z = cur
    [
        x+1,y,z
        x-1,y,z
        x,y+1,z
        x,y-1,z
        x,y,z+1
        x,y,z-1
    ]

let isOut min max side =
    let x, y, z = side
    x > max || x < min || y > max || y < min || z > max || z < min

let rec searchPath isOut path visited =
    let notVisited side = Set.contains side visited |> not

    if Set.isEmpty path then None
        else
    let side = Set.maxElement path
    if isOut side then Some side
    else
        let moves = neighbours side
        let path = moves |> List.filter notVisited |> Set.ofList |> Set.union path |> Set.remove side
        let visited = Set.add side visited
        searchPath isOut path visited


let isReachable isOut cubes side =
    if Set.contains side cubes then false
    else
    let initPath = Set.singleton side
    searchPath isOut initPath cubes |> Option.isSome

[<Puzzle(2022, 18)>]
let puzzle case (source:seq<string>) =
    let cubes = Seq.map parseCube source |> Set.ofSeq
    let sides = cubes|> Seq.collect neighbours

    let contains side = Set.contains side cubes

    match case with
    | Case.A ->
        sides
        |> Seq.filter (contains >> not)
        |> Seq.length
    | Case.B ->
        let maxCube = cubes |> Seq.map (fun (x,y,z) -> max x y |> max z) |> Seq.max
        let minCube = cubes |> Seq.map (fun (x,y,z) -> min x y |> min z) |> Seq.min
        let isOut = isOut minCube maxCube
        let isReachable = isReachable isOut cubes

        let covered side =
            if Set.contains side cubes then 0
            else neighbours side |> List.filter (fun v -> Set.contains v cubes) |> List.length

        sides
        |> Seq.filter isReachable
        |> Seq.length



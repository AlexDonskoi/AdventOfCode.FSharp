module AdventOfCode.Cases.Y2021.Day22

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type State = | On | Off
type Cuboid = (int*int)*(int*int)*(int*int)
type Command = State*Cuboid

let parseLimit =
    String.split "=" >> function
    | [|_; limits |] ->limits
    | _-> failwith "incorrect source"
    >> String.split ".." >> function
    | [| Int v1; Int v2 |] -> (v1, v2)
    | _-> failwith "incorrect source"
let parseLimits:string->Cuboid =
    String.split "," >> function
    | [|x; y; z |] -> (parseLimit x, parseLimit y, parseLimit z)
    | _-> failwith "incorrect source"

let parse:string->Command = String.split " " >> function
    | [| "on"; coords |] -> (On, parseLimits coords)
    | [| "off"; coords |] -> (Off, parseLimits coords)
    | _ -> failwith "incorrect source"



let wraps (tgt1, tgt2) (src1, src2) = src1<= tgt1 && src2 >= tgt2
let wraps3D (srcX, srcY, srcZ) (tgtX, tgtY, tgtZ)  = wraps tgtX srcX && wraps tgtY srcY && wraps tgtZ srcZ

let split (tgtMin, tgtMax) (srcMin, srcMax) =
    if (srcMax < tgtMin) || srcMin > tgtMax then [(tgtMin, tgtMax)] // no intersection
    elif (srcMax >= tgtMax) && srcMin <= tgtMin then [(tgtMin, tgtMax)] // full covered
    elif srcMin >= tgtMin && srcMax <= tgtMax then [(tgtMin, srcMin - 1);(srcMin, srcMax); (srcMax + 1, tgtMax)] // inside
    elif srcMin < tgtMin then [(tgtMin, srcMax); (srcMax + 1, tgtMax)]
    else [(tgtMin, srcMin - 1); (srcMin, tgtMax)]
    |> List.filter (fun (v1, v2) -> v1<=v2) |> List.distinct

let split3d (tgtX, tgtY, tgtZ) (srcX, srcY, srcZ) =
    let splitX = split tgtX srcX
    let splitY = split tgtY srcY
    let splitZ = split tgtZ srcZ
    [for x in splitX do for y in splitY do for z in splitZ -> x,y,z]
    |> List.filter (wraps3D (srcX, srcY, srcZ) >> not)


let rec run cnt res (inputs:list<Command>) =
    let folder acc (_,cur) =
        List.collect (fun v -> split3d v cur) acc
    match inputs with
    | [] -> res
    | (Off, _)::rest -> run cnt res rest
    | (On, h)::rest ->
        let res = List.fold folder [h] rest |> List.fold (fun acc cur -> cnt cur + acc) res
        run cnt res rest

let countA mx (x,y,z) =
    let limit mx (x1, x2) =
        if x1 < -mx || x2 > mx then 0 else (min mx x2) - (max x1 -mx) + 1
        |> int64
    (limit mx x) * (limit mx y) * (limit mx z)

let countB (x,y,z) =
    let limit (x1, x2) = x2 - x1 + 1 |> int64
    (limit x) * (limit y) * (limit z)

[<Puzzle(2021, 22)>]
let puzzle case (input:seq<string>) =
    input
    |> Seq.map parse
    |> Seq.toList
    |>
    match case with
    | Case.A -> run (countA 50) 0L
    | Case.B -> run countB 0L



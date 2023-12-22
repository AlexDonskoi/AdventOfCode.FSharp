module AdventOfCode.Cases.Y2023.Day22
open System
open System.Diagnostics
open System.Globalization
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
type Item = | Rock | Space of int64

let parseBlock = String.split "," >> function
    | [| Int x; Int y; Int z |] -> z,x,y
    | _ -> failwith "wrong format"

let parseLine = String.split "~" >> function
    | [| a; b |] -> (parseBlock b, parseBlock a)
    | _ -> failwith "wrong format"
    
let intersects b1 b2 =
    let (_, p1x2, p1y2), (_, p1x1, p1y1) = b1
    let (_, p2x2, p2y2), (_, p2x1, p2y1)= b2
    p1x1 <= p2x2 && p1x2 >= p2x1 && p1y1 <= p2y2 && p1y2 >= p2y1
    
let fallOne (state,support,supportedBy) block =
    let  (z2, x2, y2), (z1, x1, y1) = block
    let intersect = Set.filter (intersects block) state
    let maxZ = Set.map (fun ((z, _, _), _) -> z) intersect |> (fun s -> if Set.isEmpty s then 0 else Set.maxElement s)
    let sup = Set.filter (fun ((z, _, _), _) -> z = maxZ) intersect |> Set.toList
    let diff = z1 - maxZ - 1
    let block = (z2 - diff, x2, y2), (z1 - diff, x1, y1)
    let supportedBy = Map.add block sup supportedBy
    let support = List.fold (fun acc c -> Map.change c (Option.defaultValue [] >> List.append [block] >> Some) acc) support sup
    let state = Set.add block state
    state, support, supportedBy
    
let countA (_,support,supportedBy) block =
    match Map.tryFind block support with
    | None -> 1
    | Some lst ->
        if List.forall (fun b -> Map.tryFind b supportedBy |> Option.defaultValue [] |> List.length |> (<) 1) lst then 1 else 0
        
let rec countB (state,support,supportedBy) cnt blocks =
    let searchFolder acc cur =
        match Map.tryFind cur support with
        | None -> acc
        | Some sup -> Seq.fold (fun acc c -> Set.add c acc) acc sup
            
    let probable = Seq.fold searchFolder Set.empty blocks
    let fallFilter c =
        match Map.tryFind c supportedBy with
        | None -> false
        | Some [] -> false
        | Some lst ->
            Set.isSubset (Set.ofList lst) blocks
    let falling = Set.filter fallFilter probable
    let adj = Set.count falling
    if adj = 0 then cnt else
        countB <| (state,support,supportedBy) <| cnt + adj <| falling
    
[<Puzzle(2023, 22)>]
let puzzle case (source:seq<string>) =
    let bricks = source |> Seq.map parseLine |> Seq.toList |> Set.ofList
    let state = Set.fold fallOne (Set.empty, Map.empty, Map.empty) bricks
    let bricks,_,_ = state
    let count = 
        match case with
        | Case.A -> countA state
        | Case.B -> Set.singleton >> countB state 0
    Seq.sumBy count bricks
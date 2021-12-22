module AdventOfCode.Cases.Y2021.Day19

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

type Point = list<int*int*int>

let parsePoint = String.split "," >> function
    | [| Int x; Int y; Int z |] -> x,y,z | _ -> failwith "incorrect point"

let parseScanner = String.split $"{Environment.NewLine}" >> Seq.skip 1 >> Seq.map parsePoint >> Set.ofSeq

let parse = String.split $"{Environment.NewLine}{Environment.NewLine}" >> Seq.map parseScanner

let transformations =
    let rotateAxis = [
        (fun (x,y,z) -> x,y,z)
        (fun (x,y,z) -> x,-z,y)
        (fun (x,y,z) -> x,-y,-z)
        (fun (x,y,z) -> x,z,-y)
        (fun (x,y,z) -> -x,-y,z)
        (fun (x,y,z) -> -x,z,y)
        (fun (x,y,z) -> -x,y,-z)
        (fun (x,y,z) -> -x,-z,-y)
    ]
    let orient = [
        (fun (x,y,z) -> x,y,z)
        (fun (x,y,z) -> z,x,y)
        (fun (x,y,z) -> y,z,x)
    ]
    List.allPairs orient rotateAxis |> List.map(fun pair -> fst pair >> snd pair)

let positions scanner =
    seq {
        yield! Seq.map (fun f -> List.map f scanner) transformations
    }

let intersectZone (x1, y1, z1) (x2, y2, z2) = (min x1 x2, max x1 x2), (min y1 y2, max y1 y2), (min z1 z2, max z1 z2)

let overlap (_,src) target =
    let folder acc ((x1, y1, z1), p2) =
        match acc with
        | None ->
            seq { for transform in transformations do
                    let transformTarget = Set.map transform target
                    let (x2, y2, z2) = transform p2
                    let (cx, cy, cz) = (x1 - x2, y1 - y2, z1 - z2)
                    let shiftTarget = Set.map (fun (x,y,z) -> (x + cx, y + cy, z + cz)) transformTarget
                    if Set.intersect src shiftTarget |> Set.count >= 12 then yield (cx, cy, cz),shiftTarget
                        else ()
                } |> Seq.tryHead
        | v -> v

    Seq.allPairs src target
    |> Seq.fold folder None


let rec matchAny acc scanner =
    match acc with
    | [] -> None
    | h::rest ->
        match overlap h scanner with
        | None -> matchAny rest scanner
        | h -> h

let rec normalizeRec acc rest =
    match Seq.tryHead rest with
    | None -> acc
    | Some h ->
        // search for matching with head in acc
        // if no search for matching with next
        let tail = Seq.tail rest
        match matchAny acc h with
        | None ->
            let rest = seq { yield! tail; yield h } // not matched with existing then continue later
            normalizeRec acc rest
        | Some m ->
            let acc = List.append acc [m]
            normalizeRec acc tail

let normalize relative =
    let main = Seq.head relative
    let rest = Seq.tail relative
    normalizeRec [(0,0,0),main] rest

let beacons = normalize >> Seq.collect snd >> Set.ofSeq

let scanners = normalize >> Seq.map fst >> Set.ofSeq
let manhattan ((x1, y1,z1), (x2, y2, z2)) =
    abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)

[<Puzzle(2021, 19)>]
let puzzle case (input:string) =
    input
    |> parse
    |> match case with
        | Case.A -> beacons >> Set.count
        | Case.B -> scanners >> (fun v -> Seq.allPairs v v) >> Seq.map manhattan >> Seq.max


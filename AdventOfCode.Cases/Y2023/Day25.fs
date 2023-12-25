module AdventOfCode.Cases.Y2023.Day25
open System
open System.Diagnostics
open System.Globalization
open System.Net.Sockets
open System.Numerics
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Threading.Tasks
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let parseLine = String.split ": " >> function
    | [| src; targets |] -> src, String.split " " targets
    | _ -> failwith "wtf"

let appendOption v = Option.defaultValue Set.empty >> Set.add v >> Some
    
let insertMap map k v =
    map
    |> Map.change k (appendOption v)
    |> Map.change v (appendOption k)
 
let isSameGroup map acc a b =
    let conA = Map.find a map
    let conB = Map.find b map
    if Set.intersect conA conB |> Set.count |> (>) 3 then Set.add (a,b) acc else acc
    
let allPairs map acc a lst = Set.fold (fun acc c -> Set.add (min a c, max a c) acc) acc lst
    
let rec connections map acc p1 p2 p3 tgts =
    let folder acc tgt = 
        let conn = Map.find tgt map
        let conn = if tgt = fst p1 then Set.remove <| snd p1 <| conn else conn
        let conn = if tgt = snd p1 then Set.remove <| fst p1 <| conn else conn
        let conn = if tgt = fst p2 then Set.remove <| snd p2 <| conn else conn
        let conn = if tgt = snd p2 then Set.remove <| fst p2 <| conn else conn
        let conn = if tgt = fst p3 then Set.remove <| snd p3 <| conn else conn
        let conn = if tgt = snd p3 then Set.remove <| fst p3 <| conn else conn
        Set.union acc conn
    let conn = Set.fold folder Set.empty tgts
    let add = Set.difference conn acc
    
    let acc = Set.union acc add
    if Set.isEmpty add then acc else connections map acc p1 p2 p3 add
    
let check map pairs i j k =
    let p1 = Array.get pairs i
    let p2 = Array.get pairs j
    let p3 = Array.get pairs k
    let init = Set.singleton <| fst p1
    let conn = connections map Set.empty p1 p2 p3 init
    let grpCount = Set.count conn
    let mapCount = Map.count map
    if  grpCount = mapCount then None else (mapCount - grpCount) * grpCount |> Some
    
    
[<Puzzle(2023, 25)>]
let puzzle case (source:seq<string>) =
    let folder map (s, arr) =
        Seq.fold (fun acc -> insertMap acc s) map arr
    let source = source |> Seq.map parseLine |> Seq.fold folder Map.empty
    
    let allPairs = Map.fold (fun acc k v -> allPairs source acc k v ) Set.empty source |> Set.toArray
    
    let size = Array.length allPairs |> (-) <| 1
    
    let mutable res = None
    
    for i in 0 .. size do
        for j in i + 1 .. size do
            for k in j + 1 .. size do
                res <- if Option.isSome res then res else check source allPairs i j k
                
                
    
    match case with
    | Case.A -> res
    | Case.B -> Some 0
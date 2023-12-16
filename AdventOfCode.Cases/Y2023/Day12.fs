module AdventOfCode.Cases.Y2023.Day12
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let parseSeq = String.split "," >> Array.map int >> Array.toList

let parseRow = String.split " " >> function
    | [| src; pat |] -> src |> Seq.toList, parseSeq pat
    | v -> failwith $"wtf {v}"

type Connections = | No | Left | Right | Both

let groupPattern rightConnected cur =    
    let rec groupPatternRec acc cur =
        let head = List.head acc
        if cur = 0L then
            if head = 0 then List.tail acc else acc
            |> List.rev
        else
            let rest = List.tail acc
            let acc = if cur &&& 1L = 1L then (head+1)::rest else if head <> 0 then 0::acc else acc
            let cur = cur >>> 1
            groupPatternRec acc cur
    let right = rightConnected &&& cur = rightConnected
    let left = cur &&& 1L = 1L
    let connection =
        match left, right with
        | true, true -> Both
        | _, true -> Right
        | true, _ -> Left
        | _, _ -> No
    connection, groupPatternRec [0] cur
    
let rec isMaskMatch mask cur =
    match mask with
    | [] -> cur = 0L
    | h::rest -> 
        let notAllowed = if cur &&& 1L = 1L then '.' else '#'
        let cur = if cur = 0L then 0L else cur >>> 1
        h <> notAllowed && isMaskMatch rest cur

let optionAdd v = Option.defaultValue 0L >> (+) v >> Some         
 
let rec optionsRec mask pattern leftConnect maxGrp cache cur =
        if cur < 0L then cache else
            let cache =
                if isMaskMatch mask cur |> not then cache else
                    let pattern = groupPattern leftConnect cur
                    if pattern |> snd |> List.exists ( (<) maxGrp) then cache else
                        Map.change pattern (optionAdd 1L) cache                    
            optionsRec mask pattern leftConnect maxGrp cache <| cur - 1L 

let getMask mask shift ind =
    let size = Array.length mask - 1
    let ind = ind + shift
    if ind > size then '.' else mask.[ind]
            
let options mask pattern cache =
    let max = List.max pattern
    let size = List.length mask
    let maxStep = pown 2L size - 1L
    let leftConnected = pown 2l (size - 1)    
    optionsRec mask pattern leftConnected max cache maxStep
    
let calculateA src =
    let _, pattern = src
    let cache = src ||> options <| Map.empty
    [ No; Left; Right; Both ]
    |> List.fold (fun acc c -> Map.tryFind (c, pattern) cache |> Option.defaultValue 0L |> (+) acc) 0L
 
let init = Map [(No, []), 1L]

let connect = Map [(No, []), 1L; (Both, [1]), 1L]    
    
let merge (connLeft, grpLeft) (connRight, grpRight) =
    let rgpRes =
        if List.length grpRight = 0 then grpLeft
        else if List.length grpLeft = 0  then grpRight
        else
            match connLeft, connRight with
            | Right, Left
            | Right, Both
            | Both, Left
            | Both, Both ->
                let headRight = List.head grpRight
                let grpRight = List.tail grpRight
                let revLeft = grpLeft |> List.rev
                let lastLeft = List.head revLeft
                List.concat [ revLeft |> List.tail |> List.rev ; [lastLeft + headRight] ;grpRight ]
            |_ -> List.concat [grpLeft; grpRight]
    let connection = match connRight with | Right | Both -> Right | _ -> No
    (connection, rgpRes)   

type TKey = Connections*list<int>
type TMap = Map<Connections*list<int>, int64>

let rec isSame pattern cur =
    match pattern, cur with
    | [],[] -> true
    | _, [] -> true
    | _, [_] -> true
    | [], _ -> false
    | h1::rest1, h2::rest2 -> h1 = h2 && isSame rest1 rest2
    

let mergeAdd pattern left acc keyR valR =
    let max = List.max pattern
    
    let keyL, valL = left
    let key = merge keyL keyR
    let tmpPattern = snd key
    if tmpPattern |> isSame pattern |> not then acc
    else if List.exists ((<) max) tmpPattern then acc else
        Map.change key (Option.defaultValue 0L >> (+) (valL * valR) >> Some) acc

let mergeWithMap pattern map acc keyCur valCur = Map.fold (mergeAdd pattern (keyCur,valCur)) acc map     
 
let mergeAll pattern acc left (right:TMap) = Map.fold (mergeWithMap pattern right) acc left

    
let calculateB src =
    let _, pattern = src
    let cache = src ||> options <| Map.empty
    let parts =
        [for i in 1..5 do
            yield i,connect
            yield i,cache]
        |> List.removeAt 0
    
    let pattern = List.concat [pattern;pattern;pattern;pattern;pattern]
    let folder acc (i,cur) = mergeAll pattern Map.empty acc cur
        
    let res = List.fold folder init parts
    
    [ No; Left; Right; Both ]
    |> List.fold (fun acc c -> Map.tryFind (c, pattern) res |> Option.defaultValue 0L |> (+) acc) 0L
    
[<Puzzle(2023, 12)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.map parseRow
    let calculate =           
        match case with
        | Case.A -> calculateA
        | Case.B -> calculateB
    source
    |> Seq.fold (fun acc cur -> calculate cur |> (+) acc ) 0L
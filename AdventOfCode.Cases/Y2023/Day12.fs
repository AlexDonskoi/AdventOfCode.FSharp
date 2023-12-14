module AdventOfCode.Cases.Y2023.Day12
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let parseSeq = String.split "," >> Array.map int

let parseRow = String.split " " >> function
    | [| src; pat |] -> src |> Seq.toArray, parseSeq pat
    | v -> failwith $"wtf {v}"


let chunks = 5

let leftConnected = pown 2 (chunks - 1) |> int64

type Connections = | No | Left | Right | Both

let groupPattern cur =
    
    let rec groupPatternRec acc cur =
        let head = List.head acc
        if cur = 0L then
            if head = 0 then List.tail acc else acc
            |> List.rev
            |> List.toArray
        else
            let rest = List.tail acc
            let acc = if cur &&& 1L = 1L then (head+1)::rest else if head <> 0 then 0::acc else acc
            let cur = cur >>> 1
            groupPatternRec acc cur
    let left = leftConnected &&& cur = leftConnected
    let right = cur &&& 1L = 1L
    let connection =
        match left, right with
        | true, true -> Both
        | _, true -> Right
        | true, _ -> Left
        | _, _ -> No
    connection, groupPatternRec [0] cur
    
let rec isMaskMatchRec mask size cur ind =
        if ind > size && cur = 0L then true else
            let notAllowed = if cur &&& 1L = 1L then '.' else '#'
            let cur = if cur = 0L then 0L else cur >>> 1
            mask ind <> notAllowed && isMaskMatchRec mask size cur <| ind + 1
let isMaskMatch mask tgt  =
    isMaskMatchRec mask chunks tgt 0  

let rec isSubPatternRec target source size connection ind =
    if ind > size then false
    else if Array.get target ind >= Array.get source ind then connection = Right || connection = Both
    else if target.[ind] <> source.[ind] then false
    else isSubPatternRec target source size connection <| ind + 1
        

let isSubPattern target (connection, source) =
    let size = Array.length source
    if size = 0 then true
    else if Array.length target < size then false else
        isSubPatternRec target source size connection 0   

let optionAdd v = Option.defaultValue 0L >> (+) v >> Some
         
 
let rec optionsRec mask rootPattern acc cur =
        if cur < 0L && isMaskMatch mask cur |> not then acc else
            let pattern = groupPattern cur
            let acc =
                if isSubPattern rootPattern pattern |> not then acc else
                    Map.change pattern (optionAdd 1L) acc
            optionsRec mask rootPattern acc <| cur - 1L 

let getMask mask shift ind =
    let size = Array.length mask - 1
    let ind = ind + shift
    if ind > size then '.' else mask.[ind]
 
let merge ((connLeft, grpLeft), cntLeft) ((connRight, grpRight), cntRight) =
    let rgpRes =
        if Array.length grpRight = 0 then Array.copy grpLeft
        else if Array.length grpLeft = 0  then Array.copy grpRight
        else
            match connLeft, connRight with
            | Right, Left
            | Right, Both
            | Both, Left
            | Both, Both ->
                let headRight = Array.head grpRight
                let rgpRight = Array.tail grpRight
                let size = Array.length grpLeft
                let grpLeft = Array.copy grpLeft
                grpLeft.[size-1]<-grpLeft.[size-1] + headRight
                Array.concat [ grpLeft; grpRight ]
            |_ -> Array.concat [grpLeft; grpRight]
    ((connRight, rgpRes), cntLeft*cntRight)
            
let options mask rootPattern =
    let init = optionsRec (getMask mask 0) rootPattern Map.empty <| (leftConnected <<< 1) - 1L
    let next = optionsRec (getMask mask chunks) rootPattern Map.empty <| (leftConnected <<< 1) - 1L
    let folder next acc grp cnt =
        let folder acc k v =
            let k, v = merge (grp, cnt) (k,v)
            Map.change k (optionAdd v) acc
        next
        |> Map.fold folder acc
    init
    |> Map.fold (folder next) Map.empty
    
[<Puzzle(2023, 12)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.map parseRow
              
    match case with
    | Case.A ->
        let options = source |> Seq.fold (fun acc cur -> cur ||> options) Map.empty
        0L
    | Case.B -> 0L
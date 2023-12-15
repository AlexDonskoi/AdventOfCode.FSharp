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
let maxConnected = leftConnected * 2L - 1L

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

let rec isSubPatternRec target source maxInd connection ind =
    if ind > maxInd then false else
        let tgt = Array.get target ind
        let src = Array.get source ind
        if ind = maxInd && tgt > src  then connection = Right || connection = Both
        else if tgt <> src then false
        else if ind < maxInd then isSubPatternRec target source maxInd connection <| ind + 1
        else true
        

let isSubPattern target (connection, source) =
    let maxInd = Array.length source - 1
    if maxInd < 0 then true
    else if Array.length target <= maxInd then false else
        isSubPatternRec target source maxInd connection 0
        
let isPattern target (_, source) =
    let maxInd = Array.length source - 1
    if Array.length target <> maxInd + 1 then false else
        isSubPatternRec target source maxInd No 0          

let optionAdd v = Option.defaultValue 0L >> (+) v >> Some         
 
let rec optionsRec mask acc cur =
        if cur < 0L then acc else
            let acc =
                if isMaskMatch mask cur |> not then acc else
                    let pattern = groupPattern cur
                    Map.change pattern (optionAdd 1L) acc                    
            optionsRec mask acc <| cur - 1L 

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
                let grpRight = Array.tail grpRight
                let size = Array.length grpLeft
                let grpLeft = Array.copy grpLeft
                grpLeft.[size-1]<-grpLeft.[size-1] + headRight
                Array.concat [ grpLeft; grpRight ]
            |_ -> Array.concat [grpLeft; grpRight]
    let connection = match connRight with | Right | Both -> Right | _ -> No
    ((connection, rgpRes), cntLeft*cntRight)
  
            
let rec options mask rootPattern acc shift =
    let filterMatch root k _ = isSubPattern root k
    let filterMatch = filterMatch rootPattern
    let getOptions shift = optionsRec (getMask mask shift) Map.empty <| maxConnected
    let folder next acc grp cnt =
        let folder acc k v =
            let k, v = merge (grp, cnt) (k,v)
            Map.change k (optionAdd v) acc
        next
        |> Map.fold folder acc
    
    let next = getOptions shift    
    
    let acc = 
        acc
        |> Map.fold (folder next) Map.empty
    let acc = acc |> Map.filter filterMatch
    if shift >= Array.length mask then acc else    
        options mask rootPattern acc <| shift + chunks    
    
let initMap = Map [(No, [||]), 1L]    
    
[<Puzzle(2023, 12)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.map parseRow
              
    match case with
    | Case.A ->
        let getMatchOptions cur =
            let mask, rootPattern = cur
            options mask rootPattern initMap 0
            |> Map.filter (fun k v -> isPattern rootPattern k)
        let options = source |> Seq.fold (fun acc cur -> getMatchOptions cur) Map.empty
        0L
    | Case.B -> 0L
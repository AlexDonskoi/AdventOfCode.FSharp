module AdventOfCode.Cases.Y2023.Day10
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
type Connect =
      | L
      | R
      | T
      | B
      
let parseCell =
     function
     | '|'  -> [T; B]
     | 'F'  -> [R; B]
     | '7'  -> [L; B]
     | 'J'  -> [L; T]
     | '-'  -> [R; L]
     | 'L'  -> [R; T]
     | _ -> []

let withStart src (i, j) =
    let (len1, len2) = Array2D.sizes src
    let connect =
        [
            if i > 0 && (src.[i - 1, j] |> List.contains B) then yield T else ()
            if i < len1 && (src.[i + 1, j] |> List.contains T) then yield B else ()
            if j > 0 && (src.[i, j - 1] |> List.contains R) then yield L else ()
            if i < len2 && (src.[i, j + 1] |> List.contains L) then yield R else ()
        ]
    src.[i, j]<-connect
    src

let parseInput src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 '.'
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v]] |> ignore
    let start = Array2D.foldi (fun a i j b -> if b = 'S' then  (i, j) else a) (0, 0) map
    
    let map = Array2D.map parseCell map |> withStart <| start
    start, map
 
let rec move src exit (mv, cur) step =
    if cur = exit && step <> 0 then step  else
        let (i, j) = cur
        let nextMv =
            cur
            ||> Array2D.get src
            |> List.filter ((<>) mv)
            |> List.tryHead
            |> Option.defaultValue mv
        let nextMv, next = 
            match nextMv with
            | R -> L,(i, j + 1)
            | L -> R, (i, j - 1)  
            | T -> B, (i - 1, j)
            | B -> T, (i + 1, j)
        move src exit (nextMv, next) <| step + 1

type Cell = | Unknown | Pipe | Free     
 
let extend src =
    let (len1, len2) = Array2D.sizes src
    Array2D.create (2*len1+3) (2*len2+3) Unknown
    
let rec syncMove src extSrc exit (mv, cur) step =
    let (i, j) = cur
    Array2D.set extSrc ( 2*i + 1)  (2*j + 1) Pipe 
    if cur = exit && step <> 0 then extSrc  else
        let nextMv =
            cur
            ||> Array2D.get src
            |> List.filter ((<>) mv)
            |> List.tryHead
            |> Option.defaultValue mv
        let nextMv, next, extCur = 
            match nextMv with
            | R -> L,(i, j + 1), (( 2*i + 1), (2*j + 2))
            | L -> R, (i, j - 1) , (( 2*i + 1),  (2*j))
            | T -> B, (i - 1, j), (( 2*i ),  (2*j + 1))
            | B -> T, (i + 1, j), (( 2*i + 2),  (2*j + 1))
        Array2D.set extSrc <|| extCur <| Pipe         
        syncMove src extSrc exit (nextMv, next) <| step + 1
     
let rec wave src visit =
    let (len1, len2) = Array2D.sizes src
    let i, j = Set.minElement visit
    src.[i, j]<-Free
    let visit = Set.remove (i,j) visit
    let folder acc cur = if Array2D.get src <|| cur |> (=) Unknown then Set.add cur acc else acc
    let visit =
        [
            if i > 0 then yield (i - 1, j) else ()
            if i < len1 then yield (i + 1, j) else ()
            if j > 0 then yield (i, j - 1) else ()
            if j < len2 then yield (i, j + 1) else ()
        ]
        |> List.fold folder visit
        |> Set.remove (i,j)
    if Set.isEmpty visit then src else wave src visit    
    
    
[<Puzzle(2023, 10)>]
let puzzle case (source:seq<string>) =
    let start, map = source |> parseInput
    let startMove = 
            match start ||> Array2D.get map |> List.head with
            | R -> L
            | L -> R
            | T -> B
            | B -> T
    match case with
        | Case.A ->  move map start (startMove, start) 0 |> (/) <| 2
        | Case.B ->
            let extSrc = extend map
            let extSrc = syncMove map extSrc start (startMove, start) 0
            let extSrc = wave extSrc <| Set.singleton (0, 0)
            let folder acc i j v =
                if v <> Unknown || (i % 2 = 0) || (j % 2) = 0 then acc else acc + 1 
            Array2D.foldi folder 0 extSrc
            
            
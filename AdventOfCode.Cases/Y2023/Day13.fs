module AdventOfCode.Cases.Y2023.Day13
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
    
let toArray src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 '.'
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v]] |> ignore
    map    
    
let parseInput = String.split $"{Environment.NewLine}{Environment.NewLine}" >> Seq.map (String.split Environment.NewLine >> toArray)

type Target = | Col of int | Row of int | No

let getDiff src1 src2 =
    Array.fold2 (fun acc a b -> if a <> b then acc + 1 else acc) 0 src1 src2 

let rec checkColSymmetry diff src col =
    let size1, size2 = Array2D.sizes src
    if col > size2 - 1 then No else
        let len = min (col + 1) (size2 - col) 
        let notMatch = [for j in 0..len-1 -> getDiff src[*,col - j] src[*, col + j + 1]] |> List.sum
        if notMatch = diff then Col (col + 1) else checkColSymmetry diff src <| col + 1        
    
let rec checkRowSymmetry diff src row =
    let size1, size2 = Array2D.sizes src
    if row > size1 - 1 then No else
        let len = min (row + 1) (size1 - row) 
        let notMatch = [for j in 0..len-1 -> getDiff src[row - j, *] src[row + j + 1,*]] |> List.sum
        if notMatch = diff then Row (row + 1) else checkRowSymmetry diff src <| row + 1    
    
let checkSymm diff src =
    match checkColSymmetry diff src 0 with
    | No -> checkRowSymmetry diff src 0
    | v -> v
    
[<Puzzle(2023, 13)>]
let puzzle case (source:string) =
    let folder acc =
        function
        | Row r -> r * 100
        | Col c -> c
        | _ -> 0
        >> (+) acc
    let source = source |> parseInput
    let checkSymm =
        match case with
        | Case.A -> 0
        | Case.B -> 1
        |> checkSymm
    source |> Seq.map checkSymm |> Seq.fold folder 0
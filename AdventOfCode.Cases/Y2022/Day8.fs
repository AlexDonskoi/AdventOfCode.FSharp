module AdventOfCode.Cases.Y2022.Day8
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
open Microsoft.Win32.SafeHandles

let parse: char[,]->int[,] = Array2D.map (fun v -> int v - 48)

let visibility collect (src, coord) =
    let folder (tree, collect) (ind, cur) =
        if cur > tree then
            let collect = coord ind |> Set.add <| collect
            cur, collect
            else tree, collect
    Seq.indexed src
    |> Seq.fold folder (-1, collect)
    |> snd

let visible (src:int[,]) =
    let len = Array2D.length1 src |> (-) <| 1
    
    seq {
            for i in 0..len do
                yield src[i,*] |> Array.toSeq, (fun j -> i,j)
                yield src[i,*] |> Seq.rev, (fun j -> i,len - j)
                yield src[*, i], (fun j -> j,i)
                yield src[*, i] |> Seq.rev, (fun j -> len - j, i)
            }
    |> Seq.fold visibility Set.empty
    
let scenicScore src (i,j) =
    let folder (tree, acc) cur =
        match tree with
        | -1 -> -1, acc
        | tree when tree > cur -> tree, acc + 1
        | _ -> -1, acc + 1
    let count tree = Seq.fold folder (tree, 0) >> snd
    let len = Array2D.length1 src |> (-) <| 1
    let tree = src[i,j]
    let left = src[i,*] |> Seq.take j |> Seq.rev |> count tree
    let right = src[i,*] |> Seq.skip (j + 1) |> count tree
    let up = src[*,j] |> Seq.take i |> Seq.rev |> count tree
    let down = src[*,j] |> Seq.skip (i + 1) |> count tree
    left * right * up * down  

let scenic src =
    Array2D.alli src
    |> List.map (scenicScore src)
    |> List.max

[<Puzzle(2022, 8)>]
let puzzle case (source:seq<string>) =
    let size = Seq.length source
    let src = array2D source |> parse
    match case with
    | Case.A -> visible src |> Set.count
    | Case.B -> scenic src






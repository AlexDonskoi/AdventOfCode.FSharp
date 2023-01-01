module AdventOfCode.Cases.Y2022.Day20
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let print source =
    //let arr =Array.sortBy snd source |> Array.map fst
    //for i in arr do
      //  printf "%i, " i
    //printfn ""
    source

let move source ind =
    let len = Array.length source - 1 |> int64
    let value, pos = Array.get source ind
    let newPos = (value % len + len + (int64 pos)) % len |> int
    //let newPos = if value < 0 then newPos - 1 else newPos
    if newPos > pos then
        Array.iteri(fun i (v, p) -> if p > pos && p <= newPos then source[i]<-(v, p - 1) ) source
        source[ind]<-value, newPos
        print source
    else if newPos < pos then
        Array.iteri(fun i (v, p) -> if p >= newPos && p < pos then source[i]<-(v, p + 1) ) source
        source[ind]<-value, newPos
        print source
    else
        print source


[<Puzzle(2022, 20)>]
let puzzle case (source:seq<string>) =
    let source = Seq.map int64 source |> Seq.mapi (fun i v -> v,i) |> Seq.toArray
    let len = Array.length source

    match case with
    | Case.A ->
        for i in 0..len - 1 do
             move source i |> ignore
    | Case.B ->
        Array.iteri (fun i (v,k) -> source[i]<-v*811589153L,k) source
        for j in 1..10 do
        for i in 0..len - 1 do
             move source i |> ignore

    let baseInd = source |> Array.find (fun (v, _) -> v = 0) |> snd
    [ 1000; 2000; 3000]
    |> List.map (fun v -> (v + baseInd) % len)
    |> List.map (fun tgt -> Array.find (fun (v, i) -> i = tgt) source |> fst)
    |> List.reduce (+)
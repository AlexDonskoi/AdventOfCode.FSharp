module AdventOfCode.Cases.Y2022.Day8
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
open Microsoft.Win32.SafeHandles

let parse: char[,]->int[,] = Array2D.map (string >> int)

let visibility acc (src, calcInd) =
    let folder (tree, collect) (ind, cur) =
        if cur > tree then
            let collect = Set.add <| calcInd ind <| collect
            cur, collect
            else tree, collect
    Seq.indexed src
    |> Seq.fold folder (-1, acc)
    |> snd

let visible (src:int[,]) =
    let len =  (-) <| Array2D.length1 src <| 1

    seq {
            for i in 0..len do
                yield src[i,*] |> Array.toSeq, (fun j -> i,j)
                yield src[i,*] |> Seq.rev, (fun j -> i,len - j)
                yield src[*, i], (fun j -> j,i)
                yield src[*, i] |> Seq.rev, (fun j -> len - j, i)
            }
    |> Seq.fold visibility Set.empty

let scenicScore (src:int[,]) (i,j) =
    let folder (tree, acc) cur =
        match tree with
        | -1 -> -1, acc
        | tree when tree > cur -> tree, acc + 1
        | _ -> -1, acc + 1
    let count tree = Seq.fold folder (tree, 0) >> snd

    let score = count src[i,j]
    seq{
        yield src[i,0..j] |> Seq.take j |> Seq.rev
        yield src[i,*] |> Seq.skip (j + 1)
        yield src[*,j] |> Seq.take i |> Seq.rev
        yield src[*,j] |> Seq.skip (i + 1)
    }
    |> Seq.map score
    |> Seq.reduce (*)

let scenic src =
    Array2D.alli src
    |> List.map (scenicScore src)
    |> List.max

[<Puzzle(2022, 8)>]
let puzzle case (source:seq<string>) =
    source
    |> array2D
    |> parse
    |>
        match case with
        | Case.A -> visible >> Set.count
        | Case.B -> scenic






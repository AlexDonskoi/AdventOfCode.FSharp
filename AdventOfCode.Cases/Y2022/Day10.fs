module AdventOfCode.Cases.Y2022.Day10

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
open Microsoft.Win32.SafeHandles

type Cmd = | Noop | Add of int

let parse src =
    match String.split " " src with
    | [| "addx"; Int v |] -> Add v
    | _ -> Noop

let append v step =
    match step with
    | 20 -> v
    | 60 -> v
    | 100 -> v
    | 140 -> v
    | 180-> v
    | 220 -> v
    | _ -> 0
    |> (*) step

let rec count step acc cur cmd =
    let head = Seq.tryHead cmd
    match head with
    | Some (Add x) ->        
        let acc = append cur step + acc
        let acc = append cur (step+ 1) + acc
        count (step + 2) acc (cur + x) <| Seq.tail cmd
    | Some Noop ->
        let acc = append cur step + acc
        count (step + 1) acc cur <| Seq.tail cmd
    | _ -> acc
   
let log (screen:string[,]) =
    let size,_ = Array2D.sizes screen
    [for i in 0..size  do
        let str = Seq.reduce (+) screen[i,*]
        Console.WriteLine(str)
        ] |> ignore
    
let rec draw (screen:string[,]) step cur cmd =
    let head = Seq.tryHead cmd
    let row = step/ 40
    match head with
    | Some (Add x) ->
        let col = step % 40
        let symbol = if abs (col - cur) <= 1 then "#" else "."
        screen[row, col] <-symbol
        let col = (step + 1)% 40
        let row = (step + 1)/ 40
        let symbol = if abs (col - cur) <= 1 then "#" else "."
        screen[row, col] <-symbol
        draw screen (step + 2) (cur + x) <| Seq.tail cmd
    | Some Noop ->
        let col = step % 40
        let symbol = if abs (col - cur) <= 1 then "#" else "."
        screen[row, col] <-symbol        
        draw screen (step + 1) cur <| Seq.tail cmd
    | _ ->
        log screen
        screen
    

    

[<Puzzle(2022, 10)>]
let puzzle case (source:seq<string>) =
    source
    |> Seq.map parse
    |>  match case with
        | Case.A -> count 1 0 1 
        | Case.B ->
            (fun src ->
                draw <| Array2D.create 6 40 "" <| 0 <| 1 <|src
                |> ignore
                0)





module AdventOfCode.Cases.Y2022.Day9

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
open Microsoft.Win32.SafeHandles

type Direction = | U | D | L | R

let parse src =
    match String.split " " src with
    | [| "U"; Int v|] -> U, v
    | [| "D"; Int v|] -> D, v
    | [| "L"; Int v|] -> L, v
    | [| "R"; Int v|] -> R, v
    | _ -> failwith "unrecognized format"
    
let moveKnot (xh, yh) (xt, yt) =
    match (xh - xt), (yh - yt) with
    | 2, 2 -> xh - 1, yh - 1
    | 2, -2 -> xh - 1, yh + 1
    | -2, 2 -> xh + 1, yh - 1
    | -2, -2 -> xh + 1, yh + 1
    | 2, _ -> xh - 1, yh
    | -2, _ -> xh + 1, yh
    | _, 2 -> xh, yh - 1
    | _, -2 -> xh, yh + 1
    | _, _ -> xt, yt
      
let log rope cmd =
    let size = 40
    let field =
         [for i in 0 .. size ->
            [for j in 0..size -> "."]]
        |> array2D
    field[15, 10] <- "s"
    [for i,(x,y) in List.indexed rope |> List.rev do field[x+15,y+10] <- if i = 0 then "H" else string (i) ] |> ignore
    
    Console.WriteLine ()
    Console.Write ("== ")
    //Console.Write (cmd)
    Console.Write ("==")
    Console.WriteLine ()
    [for i in 0..size do
        let str = Seq.reduce (+) field[*,size-i]
        Console.WriteLine(str)
        ] |> ignore
    
let rec move cmd rope history =
    let size = 40
    match cmd with
    | _, v when v <= 0 ->
        
        
        
        rope, history
    | dir, v ->
        let xh, yh = List.head rope
        let (xh, yh) =
            match dir with
            | U -> (xh, yh + 1)
            | D -> (xh, yh - 1)
            | L -> (xh - 1, yh)
            | R -> (xh + 1, yh)
        
        let folder acc cur =
            moveKnot <| List.last acc <| cur
            |> List.singleton
            |> List.append acc
        let rope = List.fold <| folder <| [(xh, yh)] <| List.tail rope    
        //log rope cmd 
        Set.add <| List.last rope <| history
        |> move (dir, v - 1) rope


[<Puzzle(2022, 9)>]
let puzzle case (source:seq<string>) =
    let rope =
        match case with
        | Case.A -> List.init 2 (fun _ -> (0, 0))
        | Case.B -> List.init 10 (fun _ -> (0, 0))
        
    let s = 
        source
        |> Seq.map parse
        |> Seq.fold (fun (rope, hist) cur-> move cur rope hist) (rope, Set.empty)
        |> snd
        |> Set.count
    s






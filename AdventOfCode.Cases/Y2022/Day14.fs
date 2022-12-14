module AdventOfCode.Cases.Y2022.Day14
open System.Collections.Generic
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let parseDirection src =
    match String.split "," src with
    | [| Int x; Int y |] -> x,y
    | _ -> failwith "incorrect record"
 
let parseLine src =
    String.split " -> " src
    |> Array.map parseDirection
    |> Seq.toList
    
type Item = | Rock | Sand
type Point = | Item of option<Item> | Abyss
        
let rec fill (target:option<Item>[,]) = function
    | [] -> target
    | ((stX, stY), (fnX, fnY))::rest ->
        if stX = fnX
        then
            [for i in stY..fnY do target[i,stX]<-Some Rock] |> ignore
            [for i in fnY..stY do target[i,stX]<-Some Rock] |> ignore
            fill target rest
        else
            [for i in stX..fnX do target[stY,i]<-Some Rock] |> ignore
            [for i in fnX..stX do target[stY,i]<-Some Rock] |> ignore
            fill target rest
          
type Drop = | Point of int*int | Abyss            
let rec dropSandA (target:option<Item>[,]) (x ,y) =
    let sizey, sizex = Array2D.sizes target
    if y >= sizey then Abyss
    else
        match target[y + 1, x] with
        | None -> dropSandA target (x, y + 1)
        | Some _ ->
            if x <= 0 then Abyss
            else
                match target[y + 1, x - 1] with
                | None -> dropSandA target (x - 1, y + 1)
                | Some _ ->
                    if x >= sizex then Abyss
                    else
                        match target[y + 1, x + 1] with
                        | None -> dropSandA target (x + 1, y + 1)
                        | Some _ -> Point (x, y)
                        
let rec sandFlowA startPoint src iter =
    match dropSandA src startPoint with
    | Abyss -> iter
    | Point (x, y) ->
        Array2D.set src y x (Some Sand)
        sandFlowA startPoint src (iter + 1)        
        
let rec dropSandB (target:option<Item>[,]) (x ,y) =
    let sizey, sizex = Array2D.sizes target
    match target[y + 1, x] with
    | None -> dropSandB target (x, y + 1)
    | Some _ ->
        if x = 0 then
            let target = Array2D.init (sizey + 1) (sizex + 2) (fun y x -> if y = sizey then Some Rock else if x = 0 then None else target[y, x - 1])
            dropSandB target (1, y)
        else
            match target[y + 1, x - 1] with
            | None -> dropSandB target (x - 1, y + 1)
            | Some _ ->
                if x = sizex then
                    let target = Array2D.init (sizey + 1) (sizex + 2) (fun y x -> if y = sizey then Some Rock else target[y, x])
                    dropSandB target (x, y)
                else
                    match target[y + 1, x + 1] with
                    | None -> dropSandB target (x + 1, y + 1)
                    | Some _ -> (x, y), target
                    
let rec sandFlowB startPoint src iter =
    match dropSandB src startPoint with
    | p, _ when p = startPoint -> iter
    | (x, y), arr ->
        Array2D.set arr y x (Some Sand)
        sandFlowB startPoint arr (iter + 1)
    

        
[<Puzzle(2022, 14)>]
let puzzle case (source:seq<string>) =
    let source = Seq.map parseLine source |> Seq.toList
    let points = List.collect id source |> List.append [500, 0]
    let minX = List.map fst points |> List.min
    let maxX = List.map fst points |> List.max
    let minY = List.map snd points |> List.min
    let maxY = List.map snd points |> List.max
    let caveMap = Array2D.create (maxY - minY + 1) (maxX - minX + 1) None
    
    let normalizePoint mx my (x, y) = (x - mx, y - my)
    let normalizePair normalize (p1, p2) = (normalize p1, normalize p2)
    let normalize = normalizePair <| normalizePoint minX minY
    let lines =
        List.collect List.pairwise source
        |> List.map normalize
    let caveMap = fill caveMap lines
    
    let sandStart = normalizePoint minX minY (500, 0)
    
    match case with
    | Case.A -> sandFlowA sandStart caveMap 0
    | Case.B ->
        let floor = (maxY - minY + 1 + 2)
        let caveMap = Array2D.init floor (maxX - minX + 1) (fun y x -> if y = floor - 1 then Some Rock else if y = floor - 2 then None else caveMap[y, x])
        sandFlowB sandStart caveMap 0
        
        
    



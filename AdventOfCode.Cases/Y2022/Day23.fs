module AdventOfCode.Cases.Y2022.Day23

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

type Move = | N |S | W | E

let initState source =
    let collector (i,src)=
        src
        |> Seq.indexed
        |> Seq.map (fun (j, v) -> match v with  | '#' -> Some (i, j) | _ -> None)
        |> Seq.filter Option.isSome
        |> Seq.map (Option.defaultValue (0, 0))

    source
    |> Seq.indexed
    |> Seq.collect collector
    |> Set.ofSeq

let isAlone occupied (x, y) =
    Set.contains (x - 1, y - 1) occupied
    |> (||) (Set.contains (x - 1, y) occupied)
    |> (||) (Set.contains (x - 1, y + 1) occupied)
    |> (||) (Set.contains (x, y - 1) occupied)
    |> (||) (Set.contains (x, y + 1) occupied)
    |> (||) (Set.contains (x + 1, y - 1) occupied)
    |> (||) (Set.contains (x + 1, y) occupied)
    |> (||) (Set.contains (x + 1, y + 1) occupied)
    |> not

let move occupied (x, y) acc cur =
    match acc with
    | None ->
        match cur with
        | N -> if (Set.contains (x - 1, y - 1) occupied) || (Set.contains (x - 1, y) occupied) || (Set.contains (x - 1, y + 1) occupied) then None
                    else Some (x - 1, y)
        | S -> if (Set.contains (x + 1, y - 1) occupied) || (Set.contains (x + 1, y) occupied) || (Set.contains (x + 1, y + 1) occupied) then None
                    else Some (x + 1, y)
        | W -> if (Set.contains (x - 1, y - 1) occupied) || (Set.contains (x, y - 1) occupied) || (Set.contains (x + 1, y - 1) occupied) then None
                    else Some (x, y - 1)
        | E -> if (Set.contains (x - 1, y + 1) occupied) || (Set.contains (x, y + 1) occupied) || (Set.contains (x + 1, y + 1) occupied) then None
                    else Some (x, y + 1)

    | _ -> acc

let mergeProposals item = function
    | None -> Some [item]
    | Some lst -> item::lst |> Some

let movesFolder occupied priority proposed  pos =
    if isAlone occupied pos then proposed
    else
        let folder = move occupied pos
        match Array.fold folder None priority with
        | None -> proposed
        | Some mv ->
            Map.change mv (mergeProposals pos) proposed

let updatePriority moves =
    let cur = Array.get moves 0
    [for i in 0..2 do
         moves[i]<- moves[i + 1]] |> ignore
    moves[3]<-cur
    moves


let log state =
    let minx = Seq.map fst state |> Seq.min
    let miny = Seq.map snd state |> Seq.min
    let maxx = Seq.map fst state |> Seq.max
    let maxy = Seq.map snd state |> Seq.max
    [for x in minx..maxx do
         [for y in miny..maxy do
              let item = if Array.tryFind ((=) (x,y)) state |> Option.isSome then "#" else "."
              Console.Write(item)
              ]
         Console.WriteLine()] |> ignore
    Console.WriteLine()
    Console.WriteLine()

let rec round state priority (proposed:Map<(int*int), list<int*int>>) =
    let folder acc k = function
        | [h] -> acc |> Set.remove h |> Set.add k
        | _ -> acc

    let state = Map.fold folder state proposed
    let priority = updatePriority priority
    state, priority

let proposed state priority =
    let folder = movesFolder state priority
    state
    |> Seq.fold folder Map.empty

let rec runIter state priority = function
    | 0 ->

        //log state
        state
    | iter ->
        let proposed = proposed state priority
        //log state
        let state, priority = round state priority proposed
        runIter state priority (iter - 1)

let rec runTill state priority iter =
    match proposed state priority with
    | map when Map.isEmpty map -> iter
    | proposed ->
        //log state
        let state, priority = round state priority proposed
        runTill state priority (iter + 1)

[<Puzzle(2022, 23)>]
let puzzle case (source:seq<string>) =

    let source = initState source
    match case with
    | Case.A ->
        let state = runIter source [| N; S; W; E |] 10
        let minx = Seq.map fst state |> Seq.min
        let miny = Seq.map snd state |> Seq.min
        let maxx = Seq.map fst state |> Seq.max
        let maxy = Seq.map snd state |> Seq.max
        (maxx - minx + 1)*(maxy - miny + 1) - Seq.length state
    | Case.B ->
        runTill source [| N; S; W; E |] 1
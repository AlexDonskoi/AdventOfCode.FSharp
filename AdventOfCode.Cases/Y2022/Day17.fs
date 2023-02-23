

module AdventOfCode.Cases.Y2022.Day17
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let blocksSrc = [
    yield [
        [| false; false; true; true; true; true; false |]
    ]
    yield [
        [| false; false; false; true; false; false; false |]
        [| false; false; true; true; true; false; false |]
        [| false; false; false; true; false; false; false |]
    ]
    yield [
        [| false; false; false; false; true; false; false |]
        [| false; false; false; false; true; false; false |]
        [| false; false; true; true; true; false; false |]
    ]
    yield [
        [| false; false; true; false; false; false; false |]
        [| false; false; true; false; false; false; false |]
        [| false; false; true; false; false; false; false |]
        [| false; false; true; false; false; false; false |]
    ]
    yield [
        [| false; false; true; true; false; false; false |]
        [| false; false; true; true; false; false; false |]
    ]
]

let blockTmp = blocksSrc |> List.map (List.map Array.copy)

let moveLeft (src:list<bool[]>) =
    let canMove = src |> List.map Seq.head |> List.reduce (||) |> not
    if canMove then
        for line in src do
             for i in 0..5 do line[i]<-line[i+1]
             line[6]<-false

    src

let moveRight (src:list<bool[]>) =
    let canMove = src |> List.map Seq.last |> List.reduce (||) |> not
    if canMove then
        [for line in src do
             [for i in 6..-1..1 do line[i]<-line[i-1] ] |> ignore
             line[0]<-false
             ] |> ignore
    src

let isAllowed (state:bool[,]) src shift =
        [for ind,line in Seq.indexed src do
             let stateLine = state[shift + ind, *]
             yield! Seq.map2 (&&) stateLine line
             ] |> Seq.contains true |> not

let flowMove src flows state shift =
    let flowPointer, getCurrent = flows
    let nextPointer, current = getCurrent flowPointer
    let flow, rev =
            match current with
            | '>' -> moveRight, moveLeft
            | '<' -> moveLeft, moveRight
            | _ -> failwith "wtf"
    let src = flow src
    let src = if isAllowed state src shift then src else rev src

    let flows = nextPointer, getCurrent
    src, flows


let rec normalize state height line =
    let size = Array2D.length1 state - 1
    let res = size - line - 1

    if res > 0 then
        let mutable row = true
        for i in 0..6 do
            row <- (state[line + 1, i] || state[line, i]) && row
        if row then
            for j in 0..height + res do
                for k in 0..6 do
                    state[size - j,k]<-state[size - j - res, k]
            res
        else 0
    else 0

let getSet state height =
    let size = Array2D.length1 state - 1
    let folder acc i j v =
        if v then Set.add (i,j) acc else acc
    state[size - height..size,*]
    |> Array2D.foldi folder Set.empty

let logItem = function
    | true -> "#"
    | _ -> "."

let log (screen:int*bool[,]) =
    Console.WriteLine()
    Console.WriteLine()
    let height, screen = screen
    let len =  Array2D.length1 screen
    let shift = len - height
    for ind in shift..len-1 do
        let str = screen[ind,*]|> Seq.map logItem |> Seq.reduce (+)
        Console.WriteLine(str)

let rec fallDawn (state:int*bool[,]) (src:list<bool[]>) shift flows =
    let height, state = state
    let src, flows = flowMove src flows state shift

    if isAllowed state src (shift + 1) then
        fallDawn (height,state) src (shift + 1) flows
    else
        for ind,line in Seq.indexed src do
             let stateLine = state[shift + ind, *]
             for i in 0..6 do
                  state[shift + ind, i]<-stateLine[i] || line[i]

        let height = max height (Array2D.length1 state - shift)
        let normalized = normalize state height <| shift + Seq.length src
        let height = height - normalized
        normalized, height, state, flows

let currentBlock iter =
    let ind = iter % 5L |> int
    let src = List.item ind blocksSrc
    let tgt = List.item ind blockTmp
    for i, tgtv in List.indexed tgt do
        let srcv = List.item i src
        for j in 0..6 do
            tgtv[j]<-srcv[j]
    tgt

let rec move all sets =
    let state, flows, (cur, maxIter) = all
    let total, height, state = state

    match cur with
    | _ when cur >= maxIter -> height |> int64 |> (+) total
    | iter ->
        let currentBlock = currentBlock iter
        let shift = Array2D.length1 state |> (-) <| height + 3 + Seq.length currentBlock

        let adj, height, state, flows = fallDawn (height, state) currentBlock shift flows
        let total = int64 adj + total

        let total, cur, sets =
            if adj > 0 then
                let flowPointer, _ = flows
                let blockRef = cur % 5L |> int
                let curSet = getSet state height, flowPointer, blockRef
                match Map.tryFind curSet sets with
                | None ->
                    let sets = Map.add curSet (cur, total) sets
                    total, cur, sets
                | Some (matchIter, matchTotal) ->
                    let repeatIter = cur - matchIter
                    let repeatTimes = (maxIter - cur) / repeatIter
                    let total = repeatTimes * (total - matchTotal) + total
                    total, (repeatTimes * repeatIter + cur), Map.empty
            else total, cur, sets

        move  ((total, height, state), flows, (cur + 1L, maxIter)) sets

let maxSize = 5000

[<Puzzle(2022, 17)>]
let puzzle case (source:string) =
    let flowsSource = Seq.toList source
    let flowSize = List.length flowsSource
    let getCurrentFlow iter =
        let cur = iter % flowSize |> List.item <| flowsSource
        (iter + 1) % flowSize, cur
    let flows = 0, getCurrentFlow

    let window = Array2D.create maxSize 7 false
    let iter =
        match case with
        | Case.A -> 2022L
        | Case.B -> 1000000000000L

    [for i in 0..6 do
         window[maxSize - 1, i]<-true] |> ignore
    let state = 0L, 1, window
    move (state, flows,  (0L, iter)) Map.empty
    |> (-) <| 1L


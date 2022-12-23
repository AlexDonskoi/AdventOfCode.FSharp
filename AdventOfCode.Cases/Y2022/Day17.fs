module AdventOfCode.Cases.Y2022.Day17
open System
open System.Collections.Generic
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let rec blocks = seq {
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
    yield! blocks
}

// let moveLeft (src:list<bool[]>) =
//     let canMove = src |> List.map Seq.head |> List.reduce (||) |> not
//     if canMove then
//         [for line in src do
//              [for i in 0..5 do line[i]<-line[i+1] ] |> ignore
//              line[6]<-false
//              ] |> ignore
//     src
//
// let moveRight (src:list<bool[]>) =
//     let canMove = src |> List.map Seq.last |> List.reduce (||) |> not
//     if canMove then
//         [for line in src do
//              [for i in 6..-1..1 do line[i]<-line[i-1] ] |> ignore
//              line[0]<-false
//              ] |> ignore
//     src
//
// let isAllowed state src shift =
//         [for ind,line in Seq.indexed src do
//              let stateLine = List.skip (shift + ind) state |> Seq.head
//              yield! Seq.allPairs stateLine line |> Seq.map (fun (f,s)-> f && s)
//              ] |> Seq.contains true |> not
//
// let rec fallDawn (state:int*list<bool[]>) (src:list<bool[]>) shift flows =
//     let height, state = state
//     if isAllowed state src (shift + 1) then
//         let src =
//             src
//             |> match Seq.head flows with
//                 | '>' -> moveRight
//                 | '<' -> moveLeft
//                 | _ -> failwith "wtf"
//
//         let flows = Seq.tail flows
//         fallDawn (height,state) src (shift + 1) flows
//     else
//         [for ind,line in Seq.indexed src do
//              let stateLine = List.skip (shift + ind) state |> Seq.head
//              [for i in 0..6 do
//                   stateLine[i]<-stateLine[i] || line[i]]
//              ] |> ignore
//         let height = max height ()
//         height, state, flows
//
// let logItem = function
//     | true -> "#"
//     | _ -> "."
//
// let log (screen:list<bool[]>) =
//     Console.WriteLine()
//     Console.WriteLine()
//     [for line in screen do
//         let str = line |> Seq.map logItem |> Seq.reduce (+)
//         Console.WriteLine(str)
//
//     ] |> ignore
//
// let rec move (state:int*list<bool[]>) blocks flows = function
//     | 0 -> fst state
//     | iter ->
//         Console.WriteLine (iter)
//         let height, state = state
//         let currentBlock = Seq.head blocks
//
//         let height, state, flows = fallDawn state currentBlock (height + 3 + Seq.length currentBlock) flows
//         //log state
//         move <| height, state <| Seq.tail blocks <| flows <| iter - 1


[<Puzzle(2022, 17)>]
let puzzle case (source:string) =
    let rec flows = seq{
        yield! source
        yield! flows
    }

    let state = Array2D.create 8089 7 false
    [for i in 0..6 do
         state[8090, i]<-true] |> ignore

    let state = 1, state
    match case with
    | Case.A ->
        //move state blocks flows 2022 |> fst
        0
    | Case.B -> 0


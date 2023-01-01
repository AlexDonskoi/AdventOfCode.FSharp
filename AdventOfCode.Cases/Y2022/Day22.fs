module AdventOfCode.Cases.Y2022.Day22
open System
open System.Security.AccessControl
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Direction = | Left | Right | Up | Down

type Command = | Move | TurnR | TurnL

type MapItem = | Wall | Empty | Open

let sourceRegex = Regex "(\d+|R|L)"

let parseCommand input =
    [for m in sourceRegex.Matches input do
        match m.Value with
        | Int v -> yield! [for i in 1..v -> Move]
        | "R" -> yield TurnR
        | "L" -> yield TurnL
        | _ -> failwith "unknown command"
        ]

let parseMap = function
    | ' ' -> Empty
    | '#' -> Wall
    | '.' -> Open
    | _ -> failwith "unknown symbol"

let nonEmpty = Array.findIndex ((<>) Empty)

let turnL = function | Up -> Left | Left -> Down | Down -> Right | Right -> Up
let turnR = function | Up -> Right | Right -> Down | Down -> Left | Left -> Up

let moveA map pos dir =
    let x,y = pos
    let sizex, sizey = Array2D.sizes map
    match dir with
    | Up ->
        let notEmptyInd = map[*, y] |> Array.rev |> nonEmpty |> (-) (sizex)
        let mvInd = if x = 0 then notEmptyInd else (x - 1)
        match map[mvInd,y], map[notEmptyInd,y] with
        | Open, _ -> mvInd, y
        | Empty, Open -> notEmptyInd, y
        | _,_ -> x, y
    | Down ->
        let notEmptyInd = map[*, y]  |> nonEmpty
        let mvInd = if x = sizex then notEmptyInd else (x + 1)
        match map[mvInd,y], map[notEmptyInd,y] with
        | Open, _ -> mvInd, y
        | Empty, Open -> notEmptyInd, y
        | _, _ -> x, y
    | Left ->
        let notEmptyInd = map[x, *] |> Array.rev |> nonEmpty |> (-) (sizey)
        let mvInd = if y = 0 then notEmptyInd else (y - 1)
        match map[x,mvInd], map[x,notEmptyInd] with
        | Open, _ -> x, mvInd
        | Empty, Open -> x, notEmptyInd
        | _, _ -> x, y
    | Right ->
        let notEmptyInd = map[x, *] |> nonEmpty
        let mvInd = if y = sizey then notEmptyInd else (y + 1)
        match map[x,mvInd], map[x, notEmptyInd] with
        | Open, _ -> x, mvInd
        | Empty, Open -> x, notEmptyInd
        | _, _ -> x, y

let rec moveAllA map pos dir = function
    | [] -> pos, dir
    | TurnL::rest->
        let dir = turnL dir
        moveAllA map pos dir rest
    | TurnR::rest->
        let dir = turnR dir
        moveAllA map pos dir rest
    | Move::rest ->
        let pos = moveA map pos dir
        moveAllA map pos dir rest


//   1 2
//   3
// 5 4
// 6
let next (x,y) mapi = function
    | Up when mapi = 1 -> 6, (y,0), Right
    | Up when mapi = 2 -> 6, (49,y), Up
    | Up when mapi = 3 -> 1, (49,y), Up
    | Up when mapi = 4 -> 3, (49,y), Up
    | Up when mapi = 5 -> 3, (y,0), Right
    | Up when mapi = 6 -> 5, (49,y), Up

    | Right when mapi = 1 -> 2, (x,0), Right
    | Right when mapi = 2 -> 4, (49-x,49), Left
    | Right when mapi = 3 -> 2, (49,x), Up
    | Right when mapi = 4 -> 2, (49 - x,49), Left
    | Right when mapi = 5 -> 4, (x,0), Right
    | Right when mapi = 6 -> 4, (49,x), Up

    | Down when mapi = 1 -> 3, (0,y), Down
    | Down when mapi = 2 -> 3, (y,49), Left
    | Down when mapi = 3 -> 4, (0,y), Down
    | Down when mapi = 4 -> 6, (y,49), Left
    | Down when mapi = 5 -> 6, (0,y), Down
    | Down when mapi = 6 -> 2, (0,y), Down

    | Left when mapi = 1 -> 5, (49 - x, 0), Right
    | Left when mapi = 2 -> 1, (x,49), Left
    | Left when mapi = 3 -> 5, (0,x), Down
    | Left when mapi = 4 -> 5, (x,49), Left
    | Left when mapi = 5 -> 1, (49-x,0), Right
    | Left when mapi = 6 -> 1, (0,x), Down

    |_ -> failwith "wtf"

let moveB (maps:MapItem[,][]) mapi pos dir =
    let x,y = pos
    let next = next pos mapi dir
    //let map = Array.get maps mapi

    let next =
        match dir with
        | Up -> if x > 0 then mapi, (x - 1, y), dir else next
        | Down -> if x < 49 then mapi, (x + 1, y), dir else next
        | Left -> if y > 0 then mapi, (x, y - 1), dir else next
        | Right -> if y < 49 then mapi, (x, y + 1), dir else next
    let nMapi, (nx, xy), nDir = next
    if maps[nMapi - 1][nx, xy] = Open then nMapi, (nx, xy), nDir else mapi, (x,y), dir

let rec moveAllB maps mapi pos dir = function
    | [] -> mapi, pos, dir
    | TurnL::rest->
        let dir = turnL dir
        moveAllB maps mapi pos dir rest
    | TurnR::rest->
        let dir = turnR dir
        moveAllB maps mapi pos dir rest
    | Move::rest ->
        let mapi, pos, dir = moveB maps mapi pos dir
        moveAllB maps mapi pos dir rest

let facingScore = function
    | Up -> 3
    | Right -> 0
    | Down -> 1
    | Left -> 2

let mapiPos (x,y) = function
    | 1 -> (x, y + 50)
    | 2 -> (x, y + 100)
    | 3 -> (x + 50, y + 50)
    | 4 -> (x + 100, y + 50)
    | 5 -> (x + 100, y)
    | 6 -> (x + 150, y)
    | _ -> failwith "wtf"

[<Puzzle(2022, 22)>]
let puzzle case (source:seq<string>) =
    let map = source |> Seq.rev |>Seq.skip 1 |> Seq.rev |> Seq.toList
    let max = map |> Seq.map Seq.length |> Seq.max
    let extend max src =
        seq {
            yield! src
            yield! Seq.replicate (max - Seq.length src) ' '
        }
    let extend = extend max
    let map = map |> Seq.map extend |> Seq.toList
    let map = map |> array2D |> Array2D.map parseMap

    let commands = source |> Seq.last |> parseCommand

    let (x,y), dir =
        match case with
        | Case.A ->
            let inity = nonEmpty map[0,*]
            moveAllA map (0, inity) Right commands
        | Case.B ->
            let sides =
                [|
                    map[0..49, 50..99]
                    map[0..49, 100..149]
                    map[50..99, 50..99]
                    map[100..149, 50..99]
                    map[100..149, 0..49]
                    map[150..199, 0..49]
                |]
            let mapi, pos, dir = moveAllB sides 1 (0, 0) Right commands
            let pos = mapiPos pos mapi
            pos, dir
    printfn $"{x} {y} {dir}"
    (x + 1)*1000 + 4 * (y + 1) + (facingScore dir)
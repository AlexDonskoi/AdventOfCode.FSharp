module AdventOfCode.Cases.Y2022.Day24

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Core

// 1 - left, 2 right, 4 up, 8 down, 16 block
let initMap = function
    | '#' -> 16
    | '<' -> 1
    | '>' -> 2
    | '^' -> 4
    | 'v' -> 8
    | '.' -> 0
    | _ -> failwith "unknown symbol"

let cellState (source:int[,]) i j  =
    let leni = Array2D.length1 source - 1
    let lenj = Array2D.length2 source - 1
    if i = 0 || j = 0 || i = leni || j = lenj then source[i,j] // left borders as is
    else
        [
            if i = 1 then source[leni - 1, j] &&& 8
            if i = leni - 1 then source[1, j] &&& 4
            if j = 1 then source[i, lenj - 1] &&& 2
            if j = lenj - 1 then source[i, 1] &&& 1
            source[i - 1, j] &&& 8
            source[i + 1, j] &&& 4
            source[i, j + 1] &&& 1
            source[i, j - 1] &&& 2
        ]
        |> List.reduce (|||)

let next state =
    Array2D.init
    <| Array2D.length1 state
    <| Array2D.length2 state
    <| cellState state

let rec searchPath exit path state curStep =
    let leni = Array2D.length1 state  - 1
    let cur = Set.minElement path
    let step, (x,y) = cur
    if exit (x,y) then step, state
        else
    let state = if step > curStep then next state else state
    let path =
        [
        if x > 0 then x - 1, y
        if x < leni then x + 1, y
        x, y
        x,y - 1
        x,y + 1
        ]
        |> List.filter (fun (x,y) -> state[x,y] = 0)
        |> List.map (fun v -> step + 1, v)
        |> Set.ofSeq
        |> Set.union path
        |> Set.remove cur
    searchPath exit path state step


[<Puzzle(2022, 24)>]
let puzzle case (source:seq<string>) =

    let initState = array2D source |> Array2D.map initMap
    let leni = Array2D.length1 initState - 1
    let lenj = Array2D.length2 initState - 1
    let exit (x, y) = x = leni && y = lenj - 1
    let start (x, y) = x = 0 && y = 1

    match case with
    | Case.A ->

        let step, _ =
            searchPath
            <| exit
            <| Set.singleton (0, (0, 1))
            <| initState
            <| -1
        step
    | Case.B ->
        let step1, state =
            searchPath
            <| exit
            <| Set.singleton (0, (0, 1))
            <| initState
            <| -1

        let step2, state =
            searchPath
            <| start
            <| Set.singleton (0, (leni, lenj - 1))
            <| state
            <| -1

        let step3, _ =
            searchPath
            <| exit
            <| Set.singleton (0, (0, 1))
            <| state
            <| -1
        step1 + step2 + step3 + 1 // smth wrong with back trip
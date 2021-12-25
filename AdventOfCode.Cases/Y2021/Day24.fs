module AdventOfCode.Cases.Y2021.Day24

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Arg = | Var of int | Num of int

type Cmd = | Inp of int | Add  of int*Arg | Mul of int*Arg | Div of int*Arg| Mod of int*Arg | Eql of int*Arg

let (|IsVar|_|) = function
    | "x" -> Some 0
    | "y" -> Some 1
    | "z" -> Some 2
    | "w" -> Some 3
    |_ -> None

let (|IsArg|_|) = function
    | Int x -> Num x |> Some
    | IsVar v -> v |> Var |> Some
    | _ -> None

let parseCmd = String.split " " >> function
    | [| "inp"; IsVar v |] -> Inp v
    | [| "add"; IsVar v; IsArg a |] -> Add (v,a)
    | [| "mul"; IsVar v; IsArg a |] -> Mul (v,a)
    | [| "div"; IsVar v; IsArg a |] -> Div (v,a)
    | [| "mod"; IsVar v; IsArg a |] -> Mod (v,a)
    | [| "eql"; IsVar v; IsArg a |] -> Eql (v,a)
    | v -> failwith "unknown instruction"

let argVal vars = function
    | Num v -> v
    | Var v -> Array.get vars v

let rec exec vars inputs = function
    | [] -> Some vars
    | Inp v::rest ->
        let input = List.head inputs
        Array.set vars v input
        exec vars (List.tail inputs) rest
    | Add (v,a)::rest ->
        let arg = argVal vars a
        let tgt = Array.get vars v
        Array.set vars v <| arg + tgt
        exec vars inputs rest
    | Mul (v,a)::rest ->
        let arg = argVal vars a
        let tgt = Array.get vars v
        Array.set vars v <| arg * tgt
        exec vars inputs rest
    | Div (v,a)::rest ->
        let arg = argVal vars a
        let tgt = Array.get vars v
        if arg = 0 then None
        else
            Array.set vars v <| tgt / arg
            exec vars inputs rest
    | Mod (v,a)::rest ->
        let arg = argVal vars a
        let tgt = Array.get vars v
        if arg < 0 || tgt <= 0 then None
        else
            Array.set vars v <| tgt % arg
            exec vars inputs rest
    | Eql (v,a)::rest ->
        let arg = argVal vars a
        let tgt = Array.get vars v
        let eql = if tgt = arg then 1 else 0
        Array.set vars v eql
        exec vars inputs rest

let commands = Seq.map parseCmd >> Seq.toList

let asInputs acc num =
    match num % 10L with
    | 0L when num > 0L -> None
    | 0L -> Some acc
    | v -> List.append [int v] acc |> Some

let rec run commands input =
    let vars = Array.create 4 0
    match asInputs [] input with
    | _ when input = 11111111111111L -> None
    | None -> run commands (input - 1L)
    | Some inp ->
        match exec vars inp commands with
        | Some [| _;_;z;_ |] when z <> 0 -> Some input
        | _ -> run commands (input - 1L)

let step a1 a2 a3 w z = if z%26 + a2 <> w then 26*(z/a1) + w + a3 else (z/a1)

let steps = [|
    step 1 11 6
    step 1 11 14
    step 1 15 13
    step 26 -14 1
    step 1 10 6
    step 26 0 13
    step 26 -6 6
    step 1 13 3
    step 26 -3 8
    step 1 13 14
    step 1 15 4
    step 26 -2 7
    step 26 -9 15
    step 26 -2 1
|]

let rec decrement arr ind =
    let nextDigit v = if v = 1 then 9 else (v - 1)
    let value = Array.get arr ind |> nextDigit
    Array.set arr ind value
    if value = 9 && ind <= 0 then None
    elif value = 9 then decrement arr (ind - 1)
    else Some arr

let init = [| 9;9;9;9;9;9;9;9;9;9;9;9;9;9 |]
//let init = [| 1;1;1;1;1;1;1;1;1;9;9;9;9;9 |]

let result  value = Array.fold2 (fun z w step -> step w z) 0 value steps

let rec iterate size value =
    if result value = 0 then value |> Some
    else
        match decrement value size with
        | None -> None
        | Some v -> iterate size v

(*
1  w1 + 6
2  26*(w1+6) + w2 + 14
3  26*26*(w1+6) + 26*(w2+14) + w3 + 13
4  26*(w1+6) + (w2+14)                       w4= w3 - 1
5   26*26*(w1+6) + 26*(w2+14) + w5 + 6
6   26*(w1+6) + (w2+14)                      w6= w5 + 6
7   w1+6                                     w7= w2 + 8
8   26*(w1+6) + w8 + 3
9   w1+6                                    w9=w8
10  26*(w1+6) + w10 + 14
11  26*26*(w1+6) + 26*(w10 + 14) + w11 + 4
12  26*(w1+6) + (w10 + 14)                  w12 = w11+2
13  w1 + 6                                  w13= w10 + 5
14 0                                         w14=w1+4
*)

[<Puzzle(2021, 24)>]
let puzzle case (input:seq<string>) =
    let commands = commands input

    match case with
    | Case.A -> iterate 13 init
    | Case.B -> None
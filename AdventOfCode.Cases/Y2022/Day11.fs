module AdventOfCode.Cases.Y2022.Day11
open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
open Microsoft.Win32.SafeHandles



// Monkey 0:
//   Starting items: 79, 98
//   Operation: new = old * 19
//   Test: divisible by 23
//     If true: throw to monkey 2
//     If false: throw to monkey 3

let parseItems src =
    match String.split ":" src with
    | [| _; v |] ->
        String.split "," v |> Seq.map int64 |> Seq.toList
    | _ -> failwith "unrecognizedFormat"

let parseOperation src =
    match String.split " " src with
    | [| "Operation:"; "new"; "="; "old"; "*"; "old" |] -> (fun v -> pown v 2)
    | [| "Operation:"; "new"; "="; "old"; "*"; Int64 v  |] -> (*) v
    | [| "Operation:"; "new"; "="; "old"; "+"; Int64 v  |] -> (+) v
    | _ -> failwith "unrecognizedFormat"

let parseTest src =
    match String.split " " src with
    | [| "Test:"; "divisible"; "by"; Int64 v  |] -> v
    | _ -> failwith "unrecognizedFormat"

let parseTrueBranch src =
    match String.split " " src with
    | [| "If"; "true:"; "throw"; "to"; "monkey"; Int v  |] -> v
    | _ -> failwith "unrecognizedFormat"

let parseFalseBranch src =
    match String.split " " src with
    | [| "If"; "false:"; "throw"; "to"; "monkey"; Int v  |] -> v
    | _ -> failwith "unrecognizedFormat"

type Monkey = list<int64>*(int64->int64)*(int64->int*int64)

let parseMonkey src:Monkey =
    match String.split Environment.NewLine src with
    | [| _; items; operation; test; trueBranch; falseBranch |] ->
        let operation = parseOperation operation
        let test = parseTest test
        let trueBranch = parseTrueBranch trueBranch
        let falseBranch = parseFalseBranch falseBranch
        let throwTo worry =
            if worry % test = 0L then trueBranch, worry else falseBranch, worry
        parseItems items, operation, throwTo
    | _ -> failwith "unrecognizedFormat"

let parse src =
    src
    |> String.split $"{Environment.NewLine}{Environment.NewLine}"
    |> Seq.map parseMonkey
    |> Seq.toArray

let rec round (src:Monkey[]) (acc:int64[]) step index =
    let run acc (src:Monkey[]) index =
        let items, operation, throwTo = Array.get src index
        [for item in items do
             Array.get acc index |> (+) 1L |> Array.set acc index
             let newInd, newWorry = throwTo <| operation item
             let newItems, operation, throwTo = Array.get src newInd
             (List.append newItems [newWorry], operation,throwTo) |> Array.set src newInd
             ]|> ignore
        Array.set src index ([], operation, throwTo)
    let length = Seq.length src
    match step with
    | 0 -> acc
    | _  when index = length - 1  ->
        run acc src index
        round src acc (step - 1) 0
    | _ ->
        run acc src index
        round src acc step (index + 1)

let runA source rounds =
    let len = Seq.length source
    let operationRemap (items, operation, throwTo) =
        let operation = operation >> (fun v -> v / 3L)
        items, operation, throwTo
    round
    <| Array.map operationRemap source
    <| Array.create len 0L <| rounds <| 0

let runB source rounds =
    let len = Seq.length source
    let operationRemap (items, operation, throwTo) =
        let operation = operation >>  (fun v -> v % 9699690L)
        items, operation, throwTo
    round
    <| Array.map operationRemap source
    <| Array.create len 0L <| rounds <| 0


[<Puzzle(2022, 11)>]
let puzzle case (source:string) =

    let source = parse source
    let len = Seq.length source

    match case with
    | Case.A -> runA source 20
    | Case.B -> runB source 10000
    |> Array.sortDescending
    |> Seq.toList
    |> List.take 2
    |> Seq.reduce (*)





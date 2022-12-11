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
        String.split "," v |> Seq.map uint64 |> Seq.toList
    | _ -> failwith "unrecognizedFormat"
    
let parseOperation src =    
    match String.split " " src with
    | [| "Operation:"; "new"; "="; "old"; "*"; "old" |] -> (fun (v:uint64) -> v*v)
    | [| "Operation:"; "new"; "="; "old"; "*"; Int64 v  |] -> uint64 v |> (*)
    | [| "Operation:"; "new"; "="; "old"; "+"; Int64 v  |] -> uint64 v |> (+)
    | _ -> failwith "unrecognizedFormat"    

let parseTest src =    
    match String.split " " src with
    | [| "Test:"; "divisible"; "by"; Int64 v  |] -> (fun x -> x % (uint64 v) = 0UL)
    | _ -> failwith "unrecognizedFormat"

let parseTrueBranch src =    
    match String.split " " src with
    | [| "If"; "true:"; "throw"; "to"; "monkey"; Int v  |] -> v
    | _ -> failwith "unrecognizedFormat"

let parseFalseBranch src =    
    match String.split " " src with
    | [| "If"; "false:"; "throw"; "to"; "monkey"; Int v  |] -> v
    | _ -> failwith "unrecognizedFormat"

type Monkey = list<uint64> * (uint64->int*uint64)

let parseMonkey src:Monkey =
    match String.split Environment.NewLine src with
    | [| _; items; operation; test; trueBranch; falseBranch |] ->
        let operation = parseOperation operation
        let test = parseTest test
        let trueBranch = parseTrueBranch trueBranch
        let falseBranch = parseFalseBranch falseBranch
        let throwTo src =
            let worry = (operation src) / 3UL
            if worry|> test then trueBranch, worry else falseBranch, worry
        parseItems items, throwTo
    | _ -> failwith "unrecognizedFormat"

let parse src =
    src
    |> String.split $"{Environment.NewLine}{Environment.NewLine}"
    |> Seq.map parseMonkey
    |> Seq.toArray
let parseMonkeyB src:Monkey =
    match String.split Environment.NewLine src with
    | [| _; items; operation; test; trueBranch; falseBranch |] ->
        let operation = parseOperation operation
        let test = parseTest test
        let trueBranch = parseTrueBranch trueBranch
        let falseBranch = parseFalseBranch falseBranch
        let throwTo src =
            let worry = (operation src) % 9699690UL
            if worry|> test then trueBranch, worry else falseBranch, worry
        parseItems items, throwTo
    | _ -> failwith "unrecognizedFormat"
let parseB src =
    src
    |> String.split $"{Environment.NewLine}{Environment.NewLine}"
    |> Seq.map parseMonkeyB
    |> Seq.toArray

let rec roundA (src:Monkey[]) (acc:uint64[]) step index =
    let run acc src index =        
        let items, throwTo = Array.get src index
        [for item in items do             
             Array.get acc index |> (+) 1UL |> Array.set acc index
             let newInd, newWorry = throwTo item
             let newItems, throwTo = Array.get src newInd
             (List.append newItems [newWorry], throwTo) |> Array.set src newInd             
             ]|> ignore
        Array.set src index ([], throwTo)
    let length = Seq.length src
    match step with
    | 0 -> acc
    | _  when index = length - 1  ->
        run acc src index
        roundA src acc (step - 1) 0
    | _ ->
        run acc src index
        roundA src acc step (index + 1)    

[<Puzzle(2022, 11)>]
let puzzle case (source:string) =
    match case with
    | Case.A ->
        let source = parse source
        let len = Seq.length source
        roundA source <| Array.create len 0UL <| 20 <| 0
        |> Array.sortDescending
        |> Seq.toList
         |> List.take 2
        // |> Seq.reduce (*)
    | Case.B ->
        let source = parseB source
        let len = Seq.length source
        roundA source <| Array.create len 0UL <| 10000 <| 0
        |> Array.sortDescending
        |> Seq.toList
         |> List.take 2
        // |> Seq.reduce (*)





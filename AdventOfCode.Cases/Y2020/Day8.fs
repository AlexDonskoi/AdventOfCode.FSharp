namespace AdventOfCode.Cases.Y2020

open System
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure

module Day8 =

    type Instruction =
        | Unknown of string
        | Nop of int
        | Acc of int
        | Jmp of int

    type Result =
        | Loop of int
        | Exit of int
        | Fail of obj

    type ExecutionContext = (int * list<Instruction> * list<int>)

    let parse (input:string) =
        match input.Split(" ") with
        | [| "nop"; Parser.Int arg |] -> Nop arg
        | [| "acc"; Parser.Int arg |] -> Acc arg
        | [| "jmp"; Parser.Int arg |] -> Jmp arg
        | _ -> Unknown input

    let rec run acc (context:ExecutionContext) =
        let (ind, commands, callstack) = context
        if List.contains ind callstack
            then Loop acc
            else
                match List.tryItem ind commands with
                | Some (Unknown c) -> Fail c
                | Some command ->
                    let (newAcc, newIndex) =
                        match command with
                        | Acc adj -> acc + adj, ind + 1
                        | Jmp adj -> acc, ind + adj
                        | _ ->  acc, ind + 1
                    run newAcc (newIndex, commands, List.append callstack [ind])
                | _ when ind > 0 -> Exit acc
                | _ -> Fail "incorrect index"

    let caseA commands =
        run 0 (0, commands, [])

    let replacement = function
        | Nop arg -> Jmp arg |> Some
        | Jmp arg -> Nop arg |> Some
        | _ -> None

    let withReplacement src heads tail =
        match replacement src with
        | Some cmd ->
            let scopeCommands = List.collect id [heads; [cmd]; tail]
            match caseA scopeCommands with
            | Exit t -> Exit t
            | res -> res
        | None -> Fail "no replacement"

    let rec caseB heads = function
        | h::rest ->
            match withReplacement h heads rest with
            | Exit agg -> Exit agg
            | _ -> caseB (List.append heads [h]) rest
        | _-> Fail "no exit"


    [<Puzzle(2020, 8)>]
    let puzzle case (source:seq<string>) =
        source
        |> Seq.map parse
        |> Seq.toList
        |> match case with
            | Case.A -> caseA
            | Case.B -> caseB []
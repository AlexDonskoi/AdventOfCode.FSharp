namespace AdventOfCode.Cases.Y2020

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser

module Day18 =

    type Operation = uint64 -> uint64 -> uint64

    type Mappings = Map<char, byte * Operation>

    type Token =
        | Operation of byte * Operation
        | Operand of uint64

    let (|Token|_|) (mappings:Mappings) = function
        | CharInt v -> v |> uint64 |> Operand |> Some
        | char ->
            match Map.tryFind char mappings with
            | Some operation -> operation |> Operation |> Some
            | _ -> None
        | _ -> None

    type Group =
        | Root of list<Token>
        | Nested of Group * list<Token>

    let appendToken token group =
        let push = [token]
        match group with
        | Root stack -> List.append stack push  |> Root
        | Nested (parent,stack) -> (parent, (List.append stack push)) |> Nested

    let calculate tokens =
        let folder (acc, buffer, priority) cur  =
            match buffer, cur with
            | [Operand left], Operation (opPriority, _)  ->
                if opPriority = priority then (acc, [Operand left; cur], priority)
                    else
                        let acc = List.append buffer [cur] |> List.append acc
                        (acc, [], priority)

            | [Operand left; Operation (_, op)], Operand right  ->
                let result = op left right |> Operand
                (acc, [result], priority)
            | [], Operand _  ->
                (acc, [cur], priority)
            | bfr, itm -> failwith $"unexpected sequence {bfr}, {itm}"

        let rec calculateRec priority tokens =
            let (acc, buff, _) = List.fold folder ([], [], priority) tokens
            let applyPriorityOps = List.append acc buff
            match List.length applyPriorityOps with
            | 1 -> applyPriorityOps
            | len when len > 1 ->
                calculateRec (priority + 1uy) applyPriorityOps
            | _ -> failwith $"not supported operations {tokens}"

        match calculateRec 0uy tokens with
        | [Operand x] -> x
        | arr -> failwith $"unexpected calculate result {arr}"

    let evaluate (mappings:Mappings) (group:Group) = function
        | ' ' -> group
        | '(' -> Nested (group,[])
        | Token mappings token -> appendToken token group
        | ')' ->
            match group with
            | Root tokens -> appendToken <| Operand (calculate tokens) <| Root []
            | Nested (parent,tokens) -> appendToken <| Operand (calculate tokens)  <| parent
        | ch -> failwith $"not supported symbol {ch}"

    let evalLine folder =
        List.fold folder (Root [])
        >> function
            | Root tokens -> calculate tokens
            | v -> failwith $"unexpected result {v}"

    let run mappings =
        mappings
        |> Map.ofList
        |> evaluate
        |> evalLine
        |> Seq.map
        >> Seq.fold (+) 0UL

    let mappingCaseA =
        [
            '+',(0uy, (+))
            '*',(0uy, (*))
        ]
    let mappingCaseB =
        [
            '+',(0uy, (+))
            '*',(1uy, (*))
        ]

    [<Puzzle(2020, 18)>]
    let puzzle case (input:seq<string>) =
        input
        |> Seq.map Seq.toList
        |>
        match case with
        | Case.A -> run mappingCaseA
        | Case.B -> run mappingCaseB
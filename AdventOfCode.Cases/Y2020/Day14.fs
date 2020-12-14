namespace AdventOfCode.Cases.Y2020

open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser

module Day14 =

    type Command =
        | Mask of string
        | Mem of uint64*uint64

    let parseBitString search  =
        Seq.map (fun c -> if c = search then 1UL else 0UL)
        >> Seq.fold (fun acc cur -> acc * 2UL + cur) 0UL

    let build (matchGroups:GroupCollection):Command =
        match matchGroups.["mask"].Value with
        | "" -> Mem (matchGroups.["address"].Value |> uint64, matchGroups.["value"].Value |> uint64)
        | value -> value |> Mask

    let parse input =
        let format = Regex "^mask\s+=\s+(?<mask>[10X]+)|mem\[(?<address>\d+)\]\s+=\s+(?<value>\d+)$"
        let m = format.Match input
        if not m.Success then failwith "incorrect row format"
            else m.Groups |> build

    module CaseA =

        let maskInfo (input:string) =
            parseBitString '1' input,
            parseBitString 'X' input

        let folder ((fixValue, floatValue), map) = function
                | Mem (k,v) ->
                    let newMap = v &&& floatValue |> (+) fixValue |> Map.add k <| map
                    ((fixValue, floatValue), newMap)
                | Mask mask ->
                    let (fixValue, floatValue) = maskInfo mask
                    ((fixValue, floatValue), map)

        let run =
            let lastAddress = (pown 2UL 36) - 1UL
            Seq.fold folder ((0UL, lastAddress), Map.empty)
            >> snd
            >> Map.fold (fun acc _ v -> acc + v) 0UL

    module CaseB =

        let maskInfo input =
            let folder acc cur =
                let acc = List.map ((*) 2UL) acc
                match cur with
                | 'X' -> [1UL; 0UL]
                | _ -> [0UL]
                |> List.allPairs acc
                |> List.map (fun (a,b) -> a + b)
            let offset = parseBitString '1' input
            let mirrors =
                Seq.fold folder [0UL;] input
                |> List.map ((+) offset)
            let mask = parseBitString '0' input
            (mask, mirrors)

        let folder ((mask, mirrors), map) = function
            | Mem (k,v) ->
                let offset = k &&& mask
                let newMap =
                    mirrors
                    |> List.map ((+) offset)
                    |> List.fold (fun acc cur -> Map.add cur v acc)  map
                ((mask, mirrors), newMap)
            | Mask input ->
                (maskInfo input, map)

        let run =
            let lastAddress = (pown 2UL 36) - 1UL
            Seq.fold folder ((lastAddress, [0UL]), Map.empty)
            >> snd
            >> Map.fold (fun acc _ v -> acc + v) 0UL


    [<Puzzle(2020, 14)>]
    let puzzle case (input:seq<string>) =
        input
        |> Seq.map parse
        |>
        match case with
        | Case.A -> CaseA.run
        | Case.B -> CaseB.run



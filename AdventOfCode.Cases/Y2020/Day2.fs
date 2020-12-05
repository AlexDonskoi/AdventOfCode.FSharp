namespace AdventOfCode.Cases.Y2020

open System
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure

module Day2 =

    type Policy = { min: int; max:int; char:char }

    type Row = (Policy * string)

    let build (matchGroups:GroupCollection) = ({
                min = int matchGroups.["min"].Value
                max = int matchGroups.["max"].Value
                char = char matchGroups.["char"].Value
            },
            matchGroups.["pass"].Value)

    let parse input =
        let format = Regex "^(?<min>\\d+)-(?<max>\\d+)\s+(?<char>\\w):\s+(?<pass>.+)$"
        let m = format.Match input
        if not m.Success then None
            else Some <| build m.Groups

    let validateCount min max seq =
        let count = Seq.length seq
        count >= min && count <= max

    let validatePosition min max seq =
        let count = Seq.length <| Seq.filter (fun it -> it = min || it = max) seq
        count = 1

    let validate validator row =
        match row with
        | None -> false
        | Some (policy, input) ->
            let occurrence = Seq.mapi (fun i x-> (i + 1, x)) input
                             |> Seq.filter (fun (i,x) -> x = policy.char)
                             |> Seq.map fst
            validator policy.min policy.max occurrence

    let run validator seq =
        let predicate = validate validator
        Seq.map parse seq
        |> Seq.filter predicate
        |> Seq.length

    [<Puzzle(2020, 2)>]
    let puzzle case (source:seq<string>) =
        let validator =
            match case with
            | Case.A -> validateCount
            | Case.B -> validatePosition
        run validator source


namespace AdventOfCode.Cases.Y2020

open System
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure

type Token =
    | KeyValue of (string * string)
    | Value of string

module Day4 =

    let token (src:string) =
        match src.Split (':') with
        | [|name; value|] -> KeyValue (name, value)
        | _ -> Value src

    let tokens (src:string) =
        [ for x in src.Split (' ', StringSplitOptions.RemoveEmptyEntries) -> token x]

    let rec processRecord (src:seq<string>) =
        match Seq.tryHead src with
        | Some line ->
            let tail = Seq.tail src
            match line with
            | "" -> ([], tail)
            | value ->
                let (restTokens, unprocessedSource) = processRecord tail
                let allTokens = tokens value |> List.append <|restTokens
                (allTokens, unprocessedSource)

        | None -> ([], Seq.empty)

    let rec records (src:seq<string>) =
        seq{
        match processRecord src with
        | ([], rest) -> yield! records rest
        | (record, rest) when Seq.isEmpty rest -> yield record
        | (record, rest) ->
            yield record
            yield! records rest
        }

    let requiredTokens = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]

    let hasAll required actual =
        [ for x in actual do
            match x with
            | KeyValue (n,_) -> yield n
            | _ -> () ]
        |> List.except <| required
        |> List.length
        |> (=) 0

    let regexValidate (regex:Regex) (input:string) = regex.IsMatch(input)

    let between min max = function
        | Parser.Int v -> v >= min && v <= max
        | _ -> false

    let yearValidator min max (target:string) =
        if target.Length <> 4 then false
        else between min max target

    let heightValidator options =
        let rec loop options (tgt:string) =
            match options with
            | (m:string, min, max)::_ when tgt.EndsWith(m)
                                         && tgt.Substring(0, tgt.Length - m.Length )
                                            |> between min max -> true
            | _::tail -> loop tail tgt
            | _ -> false
        loop options

    let regexValidator pattern (input:string) = Regex(pattern).IsMatch(input)

    let oneOfValidator options tgt = List.contains tgt options

    let noopValidator _ = true

    let tokenRule = function
        | "byr" -> yearValidator 1920 2002
        | "iyr" -> yearValidator 2010 2020
        | "eyr" -> yearValidator 2020 2030
        | "hgt" -> heightValidator [("cm", 150, 193); ("in", 59, 76)]
        | "hcl" -> regexValidator "^#[0-9a-f]{6}$"
        | "ecl" -> oneOfValidator ["amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth"]
        | "pid" -> regexValidator "^\d{9}$"
        | _ -> noopValidator

    let isValidToken = function
        | Value _ -> true
        | KeyValue (key, value) -> value |> tokenRule key

    let isValidRecord tokens =
        tokens
        |> List.fold (fun acc t -> acc && isValidToken t) true

    let run case (src:seq<string>) =
        let hasRequired =
            records src
            |> Seq.filter (hasAll requiredTokens)

        match case with
        | B -> hasRequired |> Seq.filter isValidRecord
        | A -> hasRequired
        |> Seq.length

    [<Puzzle(2020, 4)>]
    let puzzle case (source:seq<string>) =
        run case source
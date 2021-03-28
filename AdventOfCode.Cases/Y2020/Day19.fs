namespace AdventOfCode.Cases.Y2020

open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser

module Day19 =

    type Rules = Map<int, string>

    let parseRule input =
        match String.splitCount ":" 2 input with
        | [|Int num; rule|] -> num,rule
        | data -> failwith $"unexpected rule format {data}"

    let buildRules =
        String.split "\n"
        >> Array.map parseRule
        >> Map.ofArray

    let replaceRule rules = function
            | Int num ->
                match Map.tryFind num rules with
                | Some expr -> if String.contains '|' expr then $"({expr})" else expr
                | None -> failwith $"missed rule #{num}"
            | str -> str

    let rec buildValidatorRegex expression (replace:string -> string):string =
        let numRegex = Regex "\d+"
        let regexResult = numRegex.Replace (expression, (fun (exprMatch:Match) -> replace exprMatch.Value))
        if regexResult = expression then expression
            else buildValidatorRegex  regexResult replace

    let multiLineMatches (regex:string) src =
        Regex.Matches(src, $"^{regex}$", RegexOptions.Multiline)

    let buildRegex startRuleRegen =
        replaceRule
        >> buildValidatorRegex startRuleRegen
        >> String.replace " " ""
        >> String.replace "\"" ""

    let matches startExpression = buildRules >> buildRegex startExpression >> multiLineMatches

    module CaseA =
        let run srcRules =
            matches "0" srcRules
            >> Seq.length

    module CaseB =

        let groupCount (regexMatch:Match) =
            Regex.captures regexMatch
            >> List.length

        let filter (regexMatch:Match):bool =
            let num42Count = groupCount regexMatch "start"
            let num31Count = groupCount regexMatch "finish"
            num42Count > num31Count

        let run srcRules =
            matches "(?<start>42)+(?<finish>31)+ " srcRules
            >> Seq.filter filter
            >> Seq.length

    [<Puzzle(2020, 19)>]
    let puzzle case (input:string) =
        input
        |> String.splitCount "\n\n" 2
        |>
        function
        | [|srcRules; srcData|] ->
            let rules = buildRules srcRules
            match case with
            | Case.A -> CaseA.run
            | Case.B -> CaseB.run
            <| srcRules
            <| srcData
        | _ -> failwith "incorrect input"
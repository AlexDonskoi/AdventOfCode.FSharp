namespace AdventOfCode.Cases.Y2020

open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open AdventOfCode.Cases.Y2020.Day9

module Day16 =

    type RuleRange = int64 * int64
    type Rule = string * list<RuleRange>
    type Ticket = list<int64>
    type Captures = (string -> list<string>)
    type Input = list<Rule> * Ticket * list<Ticket>

    let captures (matches:GroupCollection) (name:string) =
        [for c in matches.[name].Captures -> c.Value]

    let groupValue (name:string) (matches:GroupCollection) =
        matches.[name].Value

    let ruleRegexp = Regex "^(?<name>.+): (?<min>\d+)-(?<max>\d+)( or (?<min>\d+)-(?<max>\d+))*$"

    let buildRuleRange min max :RuleRange = int64 min, int64 max

    let buildRule (captures:Captures):Rule =
        let field = captures "name" |> List.head
        let ruleRanges = List.map2 buildRuleRange (captures "min") (captures "max")
        field, ruleRanges

    let parseRule input =
        let m = ruleRegexp.Match input
        if not m.Success then failwith "incorrect row format"
            else captures m.Groups |> buildRule

    let parseTicket = split "," >> Array.toList >> List.map int64


    let parseInput = function
        // skip first line before ticket data
        | [rules; [_;ownTicket]; otherTickets] ->
            rules |> List.map parseRule ,
            ownTicket |> parseTicket,
            otherTickets |> List.tail |> List.map parseTicket
        | _ -> failwith "incorrect file format"

    let isInRange num (min, max) = num >= min && num <= max

    let isRuleMatched num (rule:Rule) =
        rule |> snd |> List.map (isInRange num) |> List.fold (||) false

    let rulesMatched (rules:list<Rule>) num =
        List.filter (isRuleMatched num) rules

    module CaseA =

        let isAnyValid (rules:list<Rule>) num =
            rulesMatched rules num
            |> List.length
            |> (>) 0

        let run ((rules, _, tickets):Input) =
            tickets
            |> List.collect id
            |> List.filter (isAnyValid rules >> not)
            |> List.sum

    module CaseB =

        let applyTicketData ruleset ticket =
            let stepRuleset = List.map2 rulesMatched ruleset ticket
            match List.tryFind List.isEmpty stepRuleset with
            | None -> stepRuleset
            | _ -> ruleset

        let fieldOptions rules tickets =
            let ticketSize = tickets |> List.head |> List.length
            let initAcc = List.replicate ticketSize rules
            tickets
            |> List.fold applyTicketData initAcc
            |> List.map (List.map fst)

        let fields (fieldOptions:list<list<string>>) =
            let rec searchRecursive (matched:list<int*string>) (rest: list<int*list<string>>) =
                let folder (matched, rest) (ind, items) =
                    let stepResult = List.except (List.map snd matched) items
                    match stepResult with
                    | [] -> matched, rest
                    | [x] -> List.append matched [(ind, x)], rest
                    | arr -> matched, List.append rest [(ind, arr)]
                let (accMatched, accRest) = List.fold folder (matched, []) rest
                if accMatched = matched then matched
                    else searchRecursive accMatched accRest

            fieldOptions
            |> List.mapi (fun ind el -> ind,el)
            |> searchRecursive []

        let run ((rules, ownTicket, tickets):Input) =
            List.append tickets [ownTicket]
            |> fieldOptions rules
            |> fields
            |> List.sortBy fst
            |> List.map snd
            |> List.map2 (fun v k -> k,v) ownTicket
            |> List.filter (fun (k, _) -> k.StartsWith("departure") )
            |> List.map snd
            |> List.fold (*) 1L


    [<Puzzle(2020, 16)>]
    let puzzle case (input:seq<list<string>>) =
        input
        |> Seq.toList
        |> parseInput
        |>  match case with
            | Case.A -> CaseA.run
            | Case.B -> CaseB.run
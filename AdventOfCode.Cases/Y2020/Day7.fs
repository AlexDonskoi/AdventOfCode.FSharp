namespace AdventOfCode.Cases.Y2020

open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure

module Day7 =

    type Source = (string*int)
    type Record = (string*list<Source> )

    let captures (name:string) (matches:GroupCollection) =
        [for c in matches.[name].Captures -> c.Value]

    let build (matchGroups:GroupCollection):Record = (
                matchGroups.["target"].Value,
                List.map2 (fun q s -> q, s)
                <| captures "source" matchGroups
                <| List.map int (captures "quantity" matchGroups))

    let parse input =
        let format = Regex "^(?<target>\w+ \w+) bags contain ((?<quantity>\d+) (?<source>\w+ \w+) bags?(, |.)|no other bags.)+$"
        let m = format.Match input
        if not m.Success then failwith "incorrect row format"
            else m.Groups |> build

    let matchByKey searchKey (recordKey, _) = searchKey = recordKey

    let rec containsSpecificRecursive keys items =
            let newKeys =
                 [for (x, sources) in items do
                     for (k, _) in sources do
                         if (List.contains k keys) && not (List.contains x keys)
                            then yield x
                            else ()
                ]
            if List.length newKeys = 0
                then keys
                else containsSpecificRecursive (List.append keys newKeys) items

    let caseA (inputs:list<Record>) name =
        let search = [name]
        containsSpecificRecursive search inputs
        |> List.except search
        |> List.distinct
        |> List.length

    let rec linksSearch items search aggregate=
        let nestedSearch =
            [
              for (searchKey, searchQuantity) in search do
                    let (k, links) = List.find (matchByKey searchKey) items
                    yield! [for (k, q) in links -> (k, q * searchQuantity)]
            ]
        let merged = List.append aggregate search
        if List.length nestedSearch = 0 then merged
            else linksSearch items nestedSearch merged

    let caseB (inputs:list<Record>) name=
        linksSearch inputs [name, 1] []
        |> List.map snd |> (List.sum >> (+) -1)

    [<Puzzle(2020, 7)>]
    let puzzle case (source:seq<string>) =
        let run =
            source |> Seq.toList |> List.map parse
            |> match case with
                | Case.A -> caseA
                | Case.B -> caseB
        run "shiny gold"

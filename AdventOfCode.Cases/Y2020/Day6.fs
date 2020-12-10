namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure

module Day6 =

    let caseAMap =
        // append all chars to one collection then left only unique
        List.fold Seq.append Seq.empty >> Seq.distinct

    let caseBMap:list<string> -> seq<char> =
        // append only unique chars to one collection
        List.map Set.ofSeq >> Set.intersectMany >> Set.toSeq

    let union list1 list2 = List.append list1 list2 |> List.distinct
    //let intersect list1 list2 = [list1; list2] |> List.map Set.ofList |> Set.intersectMany |> Set.toList

    [<Puzzle(2020, 6)>]
    let puzzle case (source:seq<list<string>>) =
        source
        |> match case with
            | Case.A -> Seq.map caseAMap
            | Case.B -> Seq.map caseBMap
        |> Seq.map Seq.length
        |> Seq.fold (+) 0

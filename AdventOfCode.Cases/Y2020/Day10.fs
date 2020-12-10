namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure

module Day10 =

    let caseA list =
        let folder (dif1, dif3, prev) cur =
            match cur - prev with
            | 1 -> (dif1 + 1), dif3, cur
            | 3 -> dif1, (dif3 + 1), cur
            |_ -> dif1, dif3, cur

        let (dif1, dif3, _) =
            list
            |> List.sort
            |> List.fold folder (0, 0, 0)
        dif1 * (dif3 + 1)

    let folderB (prev:list<(int*int64)>) curr =
        let waysCount =
            prev
            |> List.filter (fun (el, _) -> curr - el <= 3 )
            |> List.sumBy snd
        let addItem = [(curr, waysCount)]
        match prev with
        | [] -> addItem
        | _::tail when List.length prev >= 3 -> List.append tail addItem
        | _ -> List.append prev addItem

    let caseB =
        List.sort
        >> List.fold folderB [(0, 1L)]
        >> List.last
        >> snd


    [<Puzzle(2020, 10)>]
    let puzzle case (input:seq<string>) =
        input
        |> Seq.map int
        |> Seq.toList
        |> match case with
            | Case.A -> caseA >> int64
            | Case.B -> caseB


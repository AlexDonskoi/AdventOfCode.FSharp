namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure

module Day24 =

    let rec reference steps (x, y): int*int =
        match steps with
        | 's'::'e'::rest -> reference rest (x + 1, y - 1)
        | 's'::'w'::rest -> reference rest (x - 1, y - 1)
        | 'n'::'e'::rest -> reference rest (x + 1, y + 1)
        | 'n'::'w'::rest -> reference rest (x - 1, y + 1)
        | 'e'::rest -> reference rest (x + 2, y)
        | 'w'::rest -> reference rest (x - 2, y)
        | _ -> (x, y)

    let initState =
        let folder map cur =
            let tile = reference <| Seq.toList cur <| (0,0)
            Map.change tile (function | Some _ -> None | None -> Some ()) map
        Seq.fold folder Map.empty

    let neighbours (x, y) = [
        x + 1, y + 1
        x + 1, y - 1
        x - 1, y + 1
        x - 1, y - 1
        x + 2, y
        x - 2, y
    ]

    let nextState startMap =
        let folder (tgt, src) (pos, grp) =
            match Map.containsKey pos src, List.length grp with
            | true, 1
            | true, 2
            | false, 2 -> Map.add pos () tgt
            | _ -> tgt
            , src
        startMap
        |> Map.toList
        |> List.collect (fst >> neighbours)
        |> List.groupBy id
        |> List.fold folder (Map.empty, startMap)
        |> fst

    let rec dayState day startMap =
        if day <= 0 then startMap
            else
                let next = nextState startMap
                dayState (day - 1) next


    [<Puzzle(2020, 24)>]
    let puzzle case (input:seq<string>) =
        input
        |>
        match case with
        | Case.A -> initState >> Map.count
        | Case.B -> initState >> dayState 100 >> Map.count
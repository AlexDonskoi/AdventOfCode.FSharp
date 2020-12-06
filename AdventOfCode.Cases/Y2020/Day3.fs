namespace AdventOfCode.Cases.Y2020
open AdventOfCode.Cases.Infrastructure

module Day3 =

    let atIndex index row =
        let list = row |> Seq.toList
        let len = list |> List.length
        list |> List.skip (index % len) |> List.head

    let stepItem ind row (right, down) =
        if ind % down = 0
        then atIndex (ind * right /down) row |> Some
        else None

    let itemsChain steps input =
        let rowItems steps ind row =
            List.map (stepItem ind row) steps

        let merge =
            List.map2 (fun acc cur -> List.append acc [cur])

        let startAcc = steps |> List.map (fun _ -> [])

        Seq.mapi (rowItems steps) input
        |> Seq.fold merge startAcc

    let isTree = function
    | Some '#' -> true
    | _ -> false


    let log a =

        printfn "%A" a
        a

    let countTrees moves =
        moves
        |> log
        |> List.filter isTree
        |> List.length

    let run steps input =
        itemsChain steps input
        |> List.map countTrees
        |> List.reduce (*)

    [<Puzzle(2020, 3)>]
    let puzzle case (source:seq<string>) =
        let validator =
            match case with
            | Case.A -> [(3, 1)]
            | Case.B -> [(1, 1); (3, 1); (5, 1); (7, 1); (1, 2);]
        run validator source
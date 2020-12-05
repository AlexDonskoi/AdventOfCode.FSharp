namespace AdventOfCode.Cases.Y2020
open AdventOfCode.Cases.Infrastructure

module Day1 =

    // len - expected number of items
    // sum - the amount they should provide
    let rec search (len, sum) = function
        | h::tail ->
            match (len - 1), (sum - h) with
            | 0, 0 -> [h] |> Some // found last element to match requirements
            | l, s when l < 0 || s < 0 -> search (len, sum) tail //continue search with original conditions
            | l, s ->
                match search (l,s) tail with //continue search
                | Some lst -> h::lst |> Some // match
                | _ -> search (len, sum) tail //continue search with original conditions
        | _ -> None

    //
    let run condition =
        search condition
         >> function
            | Some lst ->
                List.fold (*) 1 lst |> Some //product of items
            | _ -> None

    [<Puzzle(2020, 1)>]
    let puzzle case (source:seq<string>) =
        let input = source |> Seq.map int |> Seq.toList
        let condition =
            match case with
            | Case.A -> 2, 2020
            | Case.B -> 3, 2020
        run condition input







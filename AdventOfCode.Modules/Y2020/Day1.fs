namespace AdventOfCode.Modules.Y2020
open AdventOfCode.Modules.Helpers.FileReader

module Day1 =
    let inputs = lines 2020 1 |> Seq.map int

    let append x inner =
        seq {
            yield x
            yield! inner
        }

    let rec search condition (lookup:seq<int>) =
        match condition with
        | (0, 0) -> Some Seq.empty
        | (len, sum) when len < 0 || sum < 0 -> None
        | (len, sum) ->
                match Seq.tryHead lookup with
                | Some x ->
                    let tail = Seq.tail lookup
                    let nested = ((len - 1), (sum - x))
                    match search nested tail with
                    | Some seq when x <= sum -> Some <| append x seq
                    | _ -> search condition tail
                | _ -> None

    let result cur acc = cur * acc

    let case len sum lookup =
        match search (len, sum) lookup with
        | Some seq ->
            Seq.fold (fun x acc -> x * acc) 1 seq |> Some
        | _ -> None

    let caseA =
        case 2 2020 <| inputs

    let caseB =
        case 3 2020 <| inputs


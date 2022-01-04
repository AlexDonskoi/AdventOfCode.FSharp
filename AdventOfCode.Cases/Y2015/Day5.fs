module AdventOfCode.Cases.Y2015.Day5

open AdventOfCode.Cases.Infrastructure

let vowels = Set ['a';'e';'i';'o';'u']
let prohibited = Set ["ab"; "cd"; "pq"; "xy"]

let join f s = String.joinSeq "" [f;s]

let isNicePair (vow, dbl, proh) (f, s) =
    if Set.contains (join f s) prohibited then (0, false, true)
    else
        let crit1 = vow + if Set.contains s vowels then 1 else 0
        let crit2 = f = s || dbl
        crit1, crit2, proh

let critA input =
    let firstVowels = if Set.contains (Seq.head input) vowels then 1 else 0
    let vow, dbl, proh =
        Seq.pairwise input
        |> Seq.fold isNicePair (firstVowels, false, false)
    dbl && (not proh) && (vow >= 3)

let increase k =
    let update =
        function
        | Some v -> v + 1
        | None -> 1
        >> Some
    Map.change k update

let critB input =
    let input = $" {input} " |> Seq.toList
    let dbl =
        input
        |> List.pairwise
        |> List.map ((<||) join)
        |> Seq.fold (fun acc cur -> increase cur acc ) Map.empty

    let wrapFolder acc = function
        | [f;s;t] -> acc || f = t
        | _ -> acc

    let trplFolder acc = function
        | [f;s;t] when f=s && s=t -> join f s |> increase <| acc
        | _ -> acc
    let wrap = input |> List.windowed 3 |> Seq.fold wrapFolder false
    let trpl = input |> List.windowed 3 |> Seq.fold trplFolder Map.empty

    let checkFolder acc k v =
        let cnt = Map.tryFind k trpl |> Option.defaultValue 0
        acc || (v >2 || (v = 2 && cnt = 0))

    wrap && Map.fold checkFolder false dbl

[<Puzzle(2015, 5)>]
let puzzle case (input:seq<string>) =
    let folder check acc cur = if check cur then acc + 1 else acc

    match case with
    | A -> critA
    | B -> critB
    |> folder
    |> Seq.fold
    <| 0 <| input
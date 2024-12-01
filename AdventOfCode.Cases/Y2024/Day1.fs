module AdventOfCode.Cases.Y2024.Day1
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let parse = String.split " " >> function
    | [| Int f; Int s |] -> f,s
    | _ -> failwith "incorrect source"
    
let resultA src =
    let prepareSet =  Seq.indexed >> Seq.map ( fun (i,v ) -> (v,i)) >> Set.ofSeq
    let s1 = src |> Seq.map fst |> prepareSet
    let s2 = src |> Seq.map snd |> prepareSet
    
    let rec calculate s1 s2 acc =
        let m1 = Set.minElement s1
        let m2 = Set.minElement s2
        let acc = acc + abs(fst m2 - fst m1)
        let s1 = Set.remove m1 s1
        let s2 = Set.remove m2 s2
        match Set.isEmpty s1, Set.isEmpty s2 with
        | true, true -> acc
        | false, false -> calculate s1 s2 acc
        | _ -> failwith "incorrect length"
    calculate s1 s2 0
    
let resultB src =
    let mapIncrease = function
        | Some v -> Some (v + 1)
        | None -> Some 1
    let map2 = src |> Seq.map snd |> Seq.fold (fun acc n -> Map.change n mapIncrease acc) Map.empty
    src
    |> Seq.map fst
    |> Seq.map (fun n -> Map.tryFind n map2 |> Option.defaultValue 0 |> (*) n)
    |> Seq.sum
    


[<Puzzle(2024, 1)>]
let puzzle case (source:seq<string>) =
    let p = source |> Seq.map parse
    match case with
        | Case.A -> resultA p
        | Case.B -> resultB p







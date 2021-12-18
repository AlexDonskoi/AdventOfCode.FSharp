module AdventOfCode.Cases.Y2021.Day18

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

type Nums = list<int*int>

let parse input =
    let folder (acc,c) = function
        | '['-> (acc, c + 1)
        | ']'-> (acc, c - 1)
        | CharInt v -> List.append acc [v, c],c
        | _ -> (acc, c)
    Seq.fold folder ([],0) input |> fst

let rec explode continuation acc num =
    match num with
    | (_,5)::(v3,5)::(v4, c4)::rest ->
        let num = List.concat [[(0,4); (v4 + v3, c4)]; rest]
        explode continuation  [] num
    | (v1,c1)::(v2,5)::(v3,5)::(v4, c4)::rest ->
        let num = List.concat [acc; [(v1 + v2, c1); (0, 4); (v4 + v3, c4)]; rest]
        explode continuation [] num
    | (v1,c1)::(v2,5)::[ (v3,5) ] ->
        let num = List.concat [acc; [(v1 + v2, c1); (0,4)]]
        explode continuation [] num
    | (v2,5)::[ (v3,5) ] -> [(0,4)]
    | h::rest ->
        let acc = List.append acc [h]
        explode continuation acc rest
    | [] -> continuation acc

let rec split acc num =
    match num with
    | (h,c)::rest when h >= 10 && c = 4 ->
        let pair = [(h/2, c + 1);(h/2 + h%2, c + 1)]
        let num = List.concat [acc; pair; rest]
        explode (split []) [] num
    | (h,c)::rest when h >= 10 ->
        let pair = [(h/2, c + 1);(h/2 + h%2, c + 1)]
        let acc = List.concat [acc; pair; rest]
        split [] acc
    | h::rest ->
        let acc = List.append acc [h]
        split acc rest
    | [] -> acc

let reduce = explode (split []) []

let add v1 v2 =
    let increase num = List.map (fun (v,c) -> (v, c + 1)) num
    let sum = List.append <| increase v1 <| increase v2
    reduce sum

let rec magnitudeRec step acc num =
    match num with
    | [v,0] -> v
    | (v1,c1)::(v2,c2)::rest when c1 = step && c2 = step ->
        let acc = List.append acc [v1*3 + v2*2, (step - 1)]
        magnitudeRec step acc rest
    | h::rest ->
        let acc = List.append acc [h]
        magnitudeRec step acc rest
    | [] -> magnitudeRec (step - 1) [] acc


[<Puzzle(2021, 18)>]
let puzzle case (input:seq<string>) =
    let magnitude = magnitudeRec 4 []
    let maxAdd (v1,v2) =
        let mag1 = add v1 v2 |> magnitude
        let mag2 = add v2 v1 |> magnitude
        max mag1 mag2
    let rec pairs src =
        match Seq.tryHead src with
        | Some h ->
            let tail = Seq.tail src
            seq {
                yield! Seq.map (fun v -> (h,v)) src
                yield! pairs tail
            }
        | None -> Seq.empty

    input
    |> Seq.map parse
    |> match case with
        | Case.A -> Seq.reduce add >> magnitude
        | Case.B -> pairs >> Seq.map maxAdd >> Seq.max


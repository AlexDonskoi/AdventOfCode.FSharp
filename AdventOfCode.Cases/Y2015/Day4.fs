module AdventOfCode.Cases.Y2015.Day4

open AdventOfCode.Cases.Infrastructure

let hash (input:string) =
    let md5 = System.Security.Cryptography.MD5.Create()
    input
    |> System.Text.Encoding.UTF8.GetBytes
    |> md5.ComputeHash
    |> Seq.collect (fun x -> x.ToString("x2"))
    |> Seq.toList

let rec compare pattern src =
    match (pattern, src) with
    | ([], _) -> true
    | (_, []) -> false
    | (e::tp, s::ts) when e = s -> compare tp ts
    | (_, _) -> false

let rec search pattern ind prefix =
    let compare =
        hash (prefix + (string)ind)
        |> compare pattern
    if compare then ind
        else search pattern (ind + 1) prefix

let repeat ch cnt = [for _ in 1 .. cnt -> ch]

[<Puzzle(2015, 4)>]
let puzzle case (input:string) =
    repeat '0'
    <|
        match case with
        | A -> 5
        | B -> 6
    |> search
    <| 1 <| input
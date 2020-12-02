namespace AdventOfCode.Modules.Y2015

module Day4 =

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

    let repeat ch cnt = seq { 1 .. cnt } |> Seq.map (fun _ -> ch) |> Seq.toList

    let case input pattern =
        List.map (search pattern 1) input

    let caseA input =
        repeat '0' 5 |> case input

    let caseB input =
        repeat '0' 6 |> case input
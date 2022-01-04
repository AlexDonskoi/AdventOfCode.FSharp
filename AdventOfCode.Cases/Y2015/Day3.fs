module AdventOfCode.Cases.Y2015.Day3

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser

let parse = String.split "x" >> function
    | [| Int a; Int b; Int c |] -> a,b,c
    | _ -> failwith "unknown input"

let move (i,j) = function
    | '>' -> (i,j + 1)
    | '<' -> (i,j - 1)
    | 'v' -> (i + 1,j)
    | '^' -> (i - 1,j)
    | _ -> (i,j)

let private folderA (pos, all) cur =
    let pos = move pos cur
    pos, (Set.add pos all)

let private folderB (s1, s2, all) cur =
    let pos = move s1 cur
    s2, pos, (Set.add pos all)

[<Puzzle(2015, 3)>]
let puzzle case (input:string) =
    let start = (0,0)
    let state = Set.singleton start
    input
    |> match case with
        | A -> Seq.fold folderA (start, state) >> snd
        | B -> Seq.fold folderB (start, start, state) >> (fun (_,_,c)->c)
    |> Set.count


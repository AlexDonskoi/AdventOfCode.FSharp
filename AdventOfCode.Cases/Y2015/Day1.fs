module AdventOfCode.Cases.Y2015.Day1

open AdventOfCode.Cases.Infrastructure

let private adjustment = function | '(' -> 1 | ')' -> -1  | _ -> 0

let private folderA acc = adjustment >> (+) acc

let private folderB (step, pos, ans) cur =
    let pos = adjustment cur |> (+) pos
    let step = step + 1
    if ans > 0 then (step, pos, ans)
    elif pos = -1 then (step, pos, step )
    else (step, pos, ans)

[<Puzzle(2015, 1)>]
let puzzle case (input:string) =
    input
    |> match case with
        | A -> Seq.fold folderA 0
        | _ -> Seq.fold folderB (0,0,0) >> (fun (_,_,c)->c)

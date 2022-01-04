module AdventOfCode.Cases.Y2015.Day2

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser

let parse = String.split "x" >> function
    | [| Int a; Int b; Int c |] -> a,b,c
    | _ -> failwith "unknown input"

let private folderA acc (a,b,c) =
    a*b + b*c + c*a
    |> (*) 2
    |> (+)
    <| if a >= b && a >= c then b*c
        elif b >= c then a*c
        else a*b
    |> (+) acc

let private folderB acc (a,b,c) =
    if a >= b && a >= c then b + c
        elif b >= c then a + c
        else a + b
    |> (*) 2
    |> (+) (a*b*c)
    |> (+) acc

[<Puzzle(2015, 2)>]
let puzzle case (input:seq<string>) =
    let folder =
        match case with
        | A -> folderA
        | B -> folderB
    input
    |> Seq.map parse
    |> Seq.fold folder 0


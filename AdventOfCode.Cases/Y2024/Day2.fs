module AdventOfCode.Cases.Y2024.Day2
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let parseLine = String.split " " >> Seq.map int >> Seq.pairwise >> Seq.map (fun (a,b) -> a-b)
  
let safeUp n = n > 0 && n < 4
let allSafeUp  = Seq.forall safeUp

let safeDown n = n < 0 && n > -4
let allSafeDown  = Seq.forall safeDown 
    
let resultA src = allSafeUp src || allSafeDown src

let tollerate = function
    | [a;b;c] ->
        a = -1 && b = 0 && c = 1
    | _ -> false
    
let resultB src =
    resultA src
    


[<Puzzle(2024, 2)>]
let puzzle case (source:seq<string>) =
    let filter = 
        match case with
        | Case.A -> resultA
        | Case.B -> resultB
    source
     |> Seq.map parseLine
     |> Seq.filter filter
     |> Seq.length







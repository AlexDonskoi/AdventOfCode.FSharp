module AdventOfCode.Cases.Y2024.Day3
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core
  
let regexpA = Regex "mul\((?<num1>\d+),(?<num2>\d+)\)"

let regexpB = Regex $"{regexpA}|do\(\)|don't\(\)"

let mulItem mtch =
    let getgroup = Regex.groupValue mtch
    match getgroup "num1", getgroup "num2" with
    | Int num1, Int num2 -> num1 * num2
    | _ -> failwith "incorrect source"

type Instruction = Mul of int | Switch of bool 
    
let parseInstruction (mtch:Match) =
    match mtch.Groups[0].Value with
    | "do()" -> Switch true
    | "don't()" -> Switch false
    | _ -> mulItem mtch |> Mul
    
let resultA = regexpA.Matches >> Seq.sumBy mulItem


let resultB =
    let folder (acc, switch) = function
        | Mul v when switch -> acc + v, switch
        | Switch v -> acc, v
        | _ -> acc, switch
    regexpB.Matches >> Seq.map parseInstruction >> Seq.fold folder (0, true) >> fst
    


[<Puzzle(2024, 3)>]
let puzzle case (source:string) =
    source
    |>
        match case with
        | Case.A -> resultA
        | Case.B -> resultB







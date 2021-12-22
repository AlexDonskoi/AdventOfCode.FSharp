module AdventOfCode.Cases.Y2021.Day21

open System
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Player = int*int // position, score
type Dice = seq<int>*int // next, rolls count
type Game = Player*Player*Dice

let deterministicDice =
    let values = [1..100]
    Seq.unfold (function | [h] -> Some (h, values) | h::rest -> Some (h, rest) | [] -> None) values

let move (pos, score) mv =
    let pos = (mv + pos  - 1) % 10 + 1
    let score = score + pos
    pos, score

let rec next (state:Game): Game =
    let p1, p2, (dice, rolls) = state
    let dice = dice |> Seq.skip 3
    let pos,score = dice |> Seq.take 3 |> Seq.sum |> move p1
    let state = (p2, (pos, score), (dice, rolls + 3)) |> Game
    if score >= 1000 then state
     else next state

let rec deterministicGame p1 p2 =
    next ((p1,0),(p2,0),(deterministicDice, 0))

let splits = [for i in 1..3 do for j in 1..3 do for k in 1..3 do yield i + j + k] |> List.countBy id

let nextOptions state:Map<int*int,int64> =
    let moveFolder (k,v) acc (mv,mvc) =
        let mvc = int64 mvc
        let (pos,score) = move k mv
        Map.change (pos,score) (function | Some c -> Some (c + v* mvc) | None -> Some (v*mvc)) acc
    let mapFolder acc k v =
        List.fold (moveFolder (k,v)) acc splits
    Map.fold mapFolder Map.empty state

let rec stepOption state =
    let sum = Map.fold (fun acc _ v -> acc + v) 0L
    //let initState = Map [(pos,0),1]
    [
        let win, rest = Map.partition (fun (_,v) _ -> v >= 21) state

        yield (sum win, sum rest)
        if Map.count rest = 0 then ()
        else
            let nextState = nextOptions rest
            yield! stepOption nextState
    ]

let rec wins p1 p2 acc =
   match p1, p2 with
   | (w,_)::rest1, (_,l)::rest2 ->
       let acc = acc + w*l
       wins rest1 rest2 acc
   | _-> acc

let caseA p1 p2 =
    let (_,score),_ , (_,rolls) = deterministicGame p1 p2
    int64 score * int64 rolls

let caseB p1 p2 =
    let p1Steps = stepOption (Map [(p1,0),1])
    let p2Steps = stepOption (Map [(p2,0),1])
    let p1win = wins p1Steps (List.append [(0L,0L)] p2Steps) 0L
    let p2win = wins p2Steps p1Steps 0L
    max p1win p2win

[<Puzzle(2021, 21)>]
let puzzle case (input:string) =
    match case with
    | Case.A -> caseA 8 7
    | Case.B -> caseB 8 7



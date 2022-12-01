namespace AdventOfCode.Cases.Y2016
open System
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Core

module Day1 =

    type Move = R of int | L of int
    
    type Direction = N | S | E | W
    
    type State = Direction * int * int    

    let parse = function
        | 'R'::x -> x |> String.joinSeq "" |> int |> R
        | 'L'::x -> x |> String.joinSeq "" |> int |> L
        | _ -> failwith "unexpected data" 

    let folder acc cur = 
        match acc, cur with
        | (N, x, y), R v -> (E, x + v, y)
        | (N, x, y), L v -> (W, x - v, y)
        
        | (W, x, y), R v -> (N, x, y + v)
        | (W, x, y), L v -> (S, x, y - v)
        
        | (S, x, y), R v -> (W, x - v, y)
        | (S, x, y), L v -> (E, x + v, y)
        
        | (E, x, y), R v -> (S, x, y - v)
        | (E, x, y), L v -> (N, x, y + v)
        
        | _ -> failwith "unexpected state" 
    
    [<Puzzle(2016, 1)>]
    let puzzle case (source:string) =
        let (_, x, y) = String.split ", " source |> Array.map (Seq.toList >> parse) |> Array.fold folder (N, 0, 0)
        match case with
            | Case.A -> (abs x) + (abs y)
            | Case.B -> 0







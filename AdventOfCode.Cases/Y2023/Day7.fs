module AdventOfCode.Cases.Y2023.Day7
open System
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

  
let parseRow =
    String.split " "
    >> function
        | [| hand; Int64 bid |] -> hand, bid
        | v -> failwith $"invalid source {v}"
let cardsA = [ 'A'; 'K'; 'Q'; 'J'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2' ]
let cardsB = [ 'A'; 'K'; 'Q'; 'T'; '9'; '8'; '7'; '6'; '5'; '4'; '3'; '2'; 'J']            
            
let mapCards = List.rev
            >> List.indexed
            >> List.map (fun (a, b)-> (b,a))
            >> Map.ofList            


let getHandA (hand, _:int64) =
    let cards = mapCards cardsA
    let comb = hand |> Seq.groupBy id |> Seq.map (snd >> Seq.length) |> Seq.sortDescending |> Seq.toList
    let cards = Seq.map (fun k -> Map.find k cards) hand |> Seq.toList
    comb, cards
 
let getHandB (hand, _:int64) =
    let cards = mapCards cardsB
    let noJoker = String.replace "J" "" hand
    let add = Seq.length hand - Seq.length noJoker
    let comb = noJoker |> Seq.groupBy id |> Seq.map (snd >> Seq.length) |> Seq.sortDescending |> Seq.toList
    let h = Seq.tryHead comb |> Option.defaultValue 0 |> (+) add
    let tail = if Seq.isEmpty comb then [] else Seq.tail comb |> Seq.toList
    let cards = Seq.map (fun k -> Map.find k cards) hand |> Seq.toList
    (h::tail), cards 
 
let handSort getHand hand1 hand2 =
    let (comb1, card1) = getHand hand1
    let (comb2, card2) = getHand hand2
    let rec comp v1 v2 =
        match v1, v2 with
        | [], [] -> 0
        | _, [] -> 1
        | [], _ -> -1
        | h1::rest1, h2::rest2 ->
            let res = compare h1 h2
            if res = 0 then comp rest1 rest2 else res
        
    let res = comp comb1 comb2
    if res = 0 then comp card1 card2 else res

[<Puzzle(2023, 7)>]
let puzzle case (source:seq<string>) =
    let getHand =
        match case with
        | Case.A -> getHandA                        
        | Case.B -> getHandB
    
    source
    |> Seq.map parseRow
    |> Seq.sortWith (handSort getHand)
    |> Seq.mapi (fun i v -> (int64 i + 1L) * snd v)
    |> Seq.sum
    
    
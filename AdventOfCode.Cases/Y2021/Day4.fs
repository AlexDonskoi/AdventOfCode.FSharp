module AdventOfCode.Cases.Y2021.Day4

open System
open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Core

type Number =
    | Marked of int
    | Unmarked of int
type Card = Number[,]

let source = String.split ","

let log v =
    printfn $"{v}"
    v

let card =
    let row = String.split " " >> Array.map (int >> Unmarked)
    String.split Environment.NewLine >> Array.map row >> array2D

let value = function | Marked v -> v | Unmarked v -> v
let isMarked = function | Marked _ -> true | _ -> false
let rows src = [ for i in 1 .. Array2D.length1 src -> src.[*, i-1] ]
let cols src = [ for i in 1 .. Array2D.length2 src -> src.[i-1, *] ]

let allNumbers src = [ yield! rows src |>  List.map Array.toList |> List.concat]

let mark num = function | Unmarked v when v = num -> Marked v | m -> m

let bingo card : bool =
    let bingo = Array.tryFind (isMarked >> not) >> Option.isNone
    let folder acc cur = cur |> bingo |> (||) acc
    (||)
    <| List.fold folder false (rows card)
    <| List.fold folder false (cols card)

let apply num (card:Card) =
    Array2D.iteri (fun i j v -> card.[i,j] <- mark num v) card
    card

let rec game source cards =
    let select = List.head source
    let cards = List.map <| apply select <| cards
    let (win, rest) = List.partition bingo cards
    seq {
        match win with
        | [] -> ()
        | w -> yield (select, w)
        match rest with
        | [] -> ()
        | r -> yield! game <| List.tail source <| r
    }

let count card =
    card |> allNumbers |> List.filter (isMarked >>not) |> List.sumBy value

let print (src, card:Number[,]) =
    log src |> ignore
    Array2D.iter (log >> ignore) card
    (src, card)

[<Puzzle(2021, 4)>]
let puzzle case (input:string) =
    let src = input |> String.split $"{Environment.NewLine}{Environment.NewLine}" |> Array.toList
    let cards = src |> List.tail |> List.map card
    let source = src |> List.head |> source |> Array.toList |> List.map int
    game source cards
    |> match case with
        | Case.A -> Seq.tryHead
        | Case.B -> Seq.tryLast
    |>
    function
    | Some (num, [h]) ->
        h |> count |> (*) num
    | _ -> failwith "unexpected winner result"

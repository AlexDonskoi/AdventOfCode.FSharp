module AdventOfCode.Cases.Y2021.Day10

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections

type Result = | Incomplete of list<char> | Corrupted of char | Correct

let openTags = ['('; '['; '{'; '<']
let closeTags = [')'; ']'; '}'; '>']

let pairs = List.map2 (fun f s -> f,s) openTags closeTags

let getClosing tag = List.find (fst >> (=) tag) pairs |> snd
let isOpen tag = List.contains tag openTags
let isClosed tag = List.contains tag closeTags

let push tag = function
    | stack when isOpen tag -> true, tag::stack
    | h::rest when getClosing h = tag -> true, rest
    | _ -> false,[]

let rec run stack input =
    match input, stack with
    | [], [] -> Correct
    | [], stack -> stack |> List.map getClosing |> Incomplete
    | h::rest, stack ->
        let isSuccess, stack = push h stack
        if isSuccess then run stack rest
            else Corrupted h

let rec caseA =
    let score = function
        | Corrupted ')' -> 3
        | Corrupted ']' -> 57
        | Corrupted '}' -> 1197
        | Corrupted '>' -> 25137
        |_ -> 0
    Seq.sumBy score

let incompleteScore =
    let itemScore item = List.findIndex ((=) item) closeTags + 1 |> int64
    function
    | Incomplete stack ->
        stack
        |> List.map itemScore
        |> List.fold (fun acc cur -> acc * 5L + cur) 0L
    | _ -> 0L

let caseB src =
    let sorted = Seq.map incompleteScore src |> Seq.filter ((<) 0) |> Seq.sort
    let skip = Seq.length sorted / 2
    Seq.skip skip sorted |> Seq.head

[<Puzzle(2021, 10)>]
let puzzle case (input:seq<string>) =
    input
    |> Seq.map (Seq.toList >> run [])
    |> match case with
        | Case.A -> caseA >> int64
        | Case.B -> caseB

module AdventOfCode.Cases.Y2022.Day13
open System.Collections.Generic
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core


type Input = | Value of int | List of list<Input>
type Tag = | Open | Group of Input

let rec wrap stack acc =
    match stack with
    | [] -> acc, []
    | Open::rest -> acc, rest
    | Group v::rest ->
        wrap rest (v::acc)

let rec parse src stack =    
    let src = Seq.toList src
    match src with
    | [] -> stack
    | ']'::rest ->
        let input, stack = wrap stack []
        let stack = Group (List input)::stack
        parse rest stack
    | '['::rest ->
        let stack = Open::stack
        parse rest stack
    | ','::'['::rest ->
        let stack = Open::stack
        parse rest stack
    | ','::rest ->
        let stack = Group (Value 0)::stack
        parse rest stack        
    | cur::rest ->
        let cur = cur |> string |> int
        match List.head stack with
        | Open ->
            let stack = Group(Value cur)::stack
            parse rest stack
        | Group(Value last) ->
            let stack = List.tail stack
            let stack = Group(Value (last*10 + cur))::stack
            parse rest stack
        | _ -> failwith "unexpected input"

type Decision = | None | Right | Wrong

let rec compare (a:Input) (b:Input) =
    match a, b with
    | List [], List (_::rest) -> Right
    | List (_::_), List []-> Wrong
    | List (a::rest), List (b::restB) ->
        match compare a b with
        | None -> compare (List rest) (List restB)
        | v -> v     
    | Value _, List _ -> compare (List [a]) b
    | List _, Value _ -> compare a (List [b])
    | Value a, Value b when a < b -> Right
    | Value a, Value b when a > b -> Wrong
    | _, _ -> None

let parseItem src =
    src|> Seq.toList |> parse <| []
    |> function
        | [ (Group v) ] -> v
        | _ -> failwith "not wrapped"

let isRight = function
    | [l; r] ->
        let a = parseItem l
        let b = parseItem r
        match compare a b with
        | Right -> true
        | Wrong -> false
        | _ -> failwith "wtf"
    | _ -> failwith "too much lines in the group"
    

        
[<Puzzle(2022, 13)>]
let puzzle case (source:seq<list<string>>) =
    match case with
    | Case.A ->
        Seq.indexed source |> Seq.sumBy (fun (i, v)-> if isRight v then i + 1 else 0 )
        
        
    | Case.B ->
        let first = parseItem "[[2]]"
        let second = parseItem "[[6]]"
        let sort a b =
            match compare a b with | Right -> 1 | Wrong -> -1| None -> 0
        let src = 
            Seq.collect id source
            |> Seq.map parseItem
            |> Seq.append [first; second]
            |> Seq.sortWith sort
            |> Seq.rev
        (*)
        <| Seq.findIndex ( (=)first) src + 1
        <| Seq.findIndex ( (=)second) src + 1
        
    



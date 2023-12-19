namespace AdventOfCode.Cases.Infrastructure

open System
open System.Text.RegularExpressions

module Parser =
    // convenient, functional TryParse wrappers returning option<'a>
    let tryParseWith (tryParseFunc: string -> bool * _) = tryParseFunc >> function
        | true, v    -> Some v
        | false, _   -> None

    let parseDate   = tryParseWith System.DateTime.TryParse

    let parseUInt16    = tryParseWith System.UInt16.TryParse
    let parseInt    = tryParseWith System.Int32.TryParse

    let parseInt64    = tryParseWith System.Int64.TryParse

    let parseSingle = tryParseWith System.Single.TryParse
    let parseDouble = tryParseWith System.Double.TryParse

    let parseHex    = tryParseWith System.Int32.TryParse
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|)   = parseDate
    let (|UInt16|_|)    = parseUInt16
    let (|Int|_|)    = parseInt
    let (|Int64|_|)    = parseInt64
    let (|Single|_|) = parseSingle
    let (|Double|_|) = parseDouble

    let (|CharInt|_|) (tgt:char)  = tgt |> string |> parseInt

    let inline charToInt c = int c - int '0'

module Array2D =
    let sizes source =
        let size1 = Array2D.length1 source - 1
        let size2 = Array2D.length2 source - 1
        size1, size2

    let alli source =
        let size1, size2 = sizes source
        [for i in 0..size1 do
                for j in 0..size2 -> (i,j)]

    let fold folder state source=
        let mutable tmpState = state
        Array2D.iter (fun v ->
            tmpState<- folder tmpState v
            ()) source
        tmpState

    let foldi folder state source=
        let mutable tmpState = state
        Array2D.iteri (fun i j v ->
            tmpState<- folder tmpState i j v
            ()) source
        tmpState

    let collect mapper source =
        source |> alli |> List.map (fun (i,j) -> mapper i j source.[i,j])

module String =
    let split (separator:string) (target:string) = target.Split(separator, StringSplitOptions.RemoveEmptyEntries)

    let contains (pattern:char) (target:string) = target.Contains(pattern)

    let replace (pattern:string) (replacement:string) (target:string) = target.Replace(pattern, replacement)

    let splitCount (separator:string) (count:int) (target:string) = target.Split(separator, count)

    let trimChar (char:char) (value:string) = value.Trim(char)

    let joinSeq (separator:string) (list:seq<char>):string =
        match Seq.tryHead list with
        | Some h -> list |> Seq.tail |> Seq.fold (fun acc cur -> (+) <| acc + separator <| string cur) (string h)
        | _ -> ""

    let upper:string->string = Seq.map Char.ToUpper >> joinSeq ""

    let fromList:list<char>->string = List.map string >> List.reduce (+)

    let lines = split Environment.NewLine

    let toGroups = split $"{Environment.NewLine}{Environment.NewLine}" >> Array.map lines

    let reverse: (string->string) = Seq.rev >> joinSeq ""

module Option =

    let max compare =
        function
        | Some v -> max v compare
        | _ -> compare
        >> Some

    let min compare =
        function
        | Some v -> min v compare
        | _ -> compare
        >> Some



module Errors =

    let failIf message condition =
        if condition then failwith message
            else ()

module Regex =
    let captures (matches:Match) (name:string) =
        if matches.Success then [for c in matches.Groups.[name].Captures -> c.Value]
            else []

    let groupValue (source:Match) (name:string)  =
        source.Groups.[name].Value

namespace AdventOfCode.Cases.Infrastructure

open System
open System.Text.RegularExpressions

module Parser =
    // convenient, functional TryParse wrappers returning option<'a>
    let tryParseWith (tryParseFunc: string -> bool * _) = tryParseFunc >> function
        | true, v    -> Some v
        | false, _   -> None

    let parseDate   = tryParseWith System.DateTime.TryParse
    let parseInt    = tryParseWith System.Int32.TryParse

    let parseInt64    = tryParseWith System.Int64.TryParse

    let parseSingle = tryParseWith System.Single.TryParse
    let parseDouble = tryParseWith System.Double.TryParse

    let parseHex    = tryParseWith System.Int32.TryParse
    // etc.

    // active patterns for try-parsing strings
    let (|Date|_|)   = parseDate
    let (|Int|_|)    = parseInt
    let (|Int64|_|)    = parseInt64
    let (|Single|_|) = parseSingle
    let (|Double|_|) = parseDouble

    let (|CharInt|_|) (tgt:char)  = tgt |> string |> parseInt

    let inline charToInt c = int c - int '0'

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

    let fromList:list<char>->string = List.map string >> List.reduce (+)

    let lines = split "\n"

    let toGroups = split "\n\n" >> Array.map lines

    let reverse: (string->string) = Seq.rev >> joinSeq ""

module Errors =

    let failIf message condition =
        if condition then failwith message
            else ()

module Regex =
    let captures (matches:Match) (name:string) =
        [for c in matches.Groups.[name].Captures -> c.Value]

    let groupValue (name:string) (matches:GroupCollection) =
        matches.[name].Value

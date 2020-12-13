namespace AdventOfCode.Cases.Infrastructure

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

    let split (separator:string) (target:string) = target.Split(separator)

    let trim (value:string) = value.Trim()

    let lines = split "\n"

    let toGroups = split "\n\n" >> Array.map lines

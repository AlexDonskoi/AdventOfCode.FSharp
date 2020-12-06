namespace AdventOfCode.Cases.Infrastructure

type Case = A|B

type ContentType =
    | Line of seq<string>
    | Char of seq<char>
    | All of string

type AcceptType =
    | Line
    | Char
    | All


type DataSource = AcceptType -> ContentType

type PuzzleAttribute(year:int, day:int) =
    inherit System.Attribute()
    member val Year = year
    member val Day = day
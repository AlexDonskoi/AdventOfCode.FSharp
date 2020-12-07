namespace AdventOfCode.Cases.Infrastructure

type Case = A|B

type ContentType =
    | Line of seq<string>
    | Char of seq<char>
    | Group of seq<list<string>>
    | All of string

type AcceptType =
    | Line
    | Char
    | Group
    | All


type DataSource = AcceptType -> ContentType

type PuzzleAttribute(year:int, day:int) =
    inherit System.Attribute()
    member val Year = year
    member val Day = day
namespace AdventOfCode.Cases.Infrastructure

type Case = A|B

type PuzzleAttribute(year:int, day:int) =
    inherit System.Attribute()
    member val Year = year
    member val Day = day
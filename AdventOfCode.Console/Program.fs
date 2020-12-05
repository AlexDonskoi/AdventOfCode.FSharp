// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2020

let searchAssembly = typeof<PuzzleAttribute>.Assembly
let runStorage year day case =
    Storage.all searchAssembly
    |> Storage.get year day case
    <| (FileReader.lines year day)

// Define a function to construct a message to print
[<EntryPoint>]
let main argv =
    runStorage 2020 5 B
    |> printfn "Result is %O"
    0 // return an integer exit code



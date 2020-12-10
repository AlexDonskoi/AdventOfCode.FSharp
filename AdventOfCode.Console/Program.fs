// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open AdventOfCode.Cases.Infrastructure

let searchAssembly = typeof<PuzzleAttribute>.Assembly
let runStorage year day case =
    Storage.get year day case
    <| FileReader.source year day

let parse argv =
    match argv with
    | [y; d; c] -> ""
    | _ -> failwith "unexpected arguments"

// Define a function to construct a message to print

[<EntryPoint>]
let main argv =
    runStorage 2020 10 B
    |> printfn "Result is %O"
    0 // return an integer exit code



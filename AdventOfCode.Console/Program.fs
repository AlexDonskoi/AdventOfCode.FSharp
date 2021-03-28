// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open AdventOfCode.Cases.Infrastructure

let searchAssembly = typeof<PuzzleAttribute>.Assembly
let runStorage year day case =
    Storage.get year day case
    <| FileReader.source year day

let run argv =
    match argv with
    | [|Parser.Int y; Parser.Int d; "A"|] -> runStorage y d A
    | [|Parser.Int y; Parser.Int d; "B"|] -> runStorage y d B
    | _ -> failwith "unexpected arguments"

// Define a function to construct a message to print

[<EntryPoint>]
let main argv =
    run argv
    //runStorage 2020 19 A
    |> printfn "Result is %O"
    0 // return an integer exit code



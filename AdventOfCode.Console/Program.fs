// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

let runner = AdventOfCode.Modules.Y2020.Day2.caseB

// Define a function to construct a message to print
[<EntryPoint>]
let main argv =
    runner
    |> printfn "Result is %A"
    0 // return an integer exit code



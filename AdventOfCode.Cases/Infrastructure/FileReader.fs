namespace AdventOfCode.Cases.Infrastructure
open System
open System.IO

module FileReader =

    let private readAsync (filePath:string) =
        async {
            use sr = new StreamReader (filePath)
            return! sr.ReadToEndAsync() |> Async.AwaitTask
        }

    let private path year day = Path.Combine(__SOURCE_DIRECTORY__, "..", "Y" + (string)year, "Inputs", "Day" + (string)day + ".txt")

    let source year day =
        let content =
            path year day
            |> readAsync
            |> Async.RunSynchronously
        function
        | Line -> content |> Parser.lines |> Array.toSeq |> ContentType.Line
        | Char -> ContentType.Char content
        | All -> ContentType.All content
        | Group -> content |> Parser.split "\n\n" |> Array.map Parser.lines |> Array.map Seq.toList |> Array.toSeq |> ContentType.Group


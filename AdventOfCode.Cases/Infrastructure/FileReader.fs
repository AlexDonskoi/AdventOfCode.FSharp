namespace AdventOfCode.Cases.Infrastructure
open System.IO

module FileReader =

    let private toLines (content:string) =
        seq { for x in content.Split("\n") -> x }

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
        | Line -> toLines content |> ContentType.Line
        | Char -> ContentType.Char content
        | All -> ContentType.All content


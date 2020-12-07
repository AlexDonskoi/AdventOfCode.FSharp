namespace AdventOfCode.Cases.Infrastructure
open System.IO

module FileReader =

    let private readAsync (filePath:string) =
        async {
            use sr = new StreamReader (filePath)
            return! sr.ReadToEndAsync() |> Async.AwaitTask
        }

    let private path year day = Path.Combine(__SOURCE_DIRECTORY__, "..", "Y" + (string)year, "Inputs", "Day" + (string)day + ".txt")

    let private split (separator:string) (target:string) = target.Split(separator)

    let private toLines = split "\n" >> Array.toSeq

    let private toGroups = split "\n\n" >> Array.map (split "\n" >> Array.toList) >> Array.toSeq

    let source year day =
        let content =
            path year day
            |> readAsync
            |> Async.RunSynchronously
        function
        | Line -> content |> toLines |> ContentType.Line
        | Char -> ContentType.Char content
        | All -> ContentType.All content
        | Group -> content |> toGroups |> ContentType.Group


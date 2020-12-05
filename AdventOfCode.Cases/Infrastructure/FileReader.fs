namespace AdventOfCode.Cases.Infrastructure
open System.IO

type InputSource = Line | Char | Full

type Data =
    | Lines of seq<string>
    | Char of seq<string>
    | Full of string

module FileReader =

    let private toLines (content:string) =
        seq { for x in content.Split("\n") -> x }

    let private readAsync (filePath:string) =
        async {
            use sr = new StreamReader (filePath)
            let! content = sr.ReadToEndAsync() |> Async.AwaitTask
            return toLines content
        }

    let path year day = Path.Combine(__SOURCE_DIRECTORY__, "..", "Y" + (string)year, "Inputs", "Day" + (string)day + ".txt")

    let lines year day =
        path year day
        |> readAsync
        |> Async.RunSynchronously


namespace AdventOfCode.Modules.Helpers
open System.IO

module FileReader =
    let readLines (filePath:string) =
        seq {
                use sr = new StreamReader (filePath)
                while not sr.EndOfStream do
                    yield sr.ReadLine ()
            }
    let path year day = Path.Combine(__SOURCE_DIRECTORY__, "..", "Y" + (string)year, "Inputs", "Day" + (string)day + ".txt")

    let lines year day =
        path year day
        |> readLines
        |> Seq.cache


namespace AdventOfCode.Cases.Infrastructure

type Message =
    | Intro
    | Welcome
    | Goodbye
    | MissedCommand
    | UnknownCommand
    | UnsupportedArgument
    | Exception
    | NotFoundValidation
    | PastValidation
    | FutureValidation
    | RepeatUnknown of int
    | RepeatCommand of string * int


type Command =
    | Quit
    | Help
    | About
    | Run
    | Cd
    | Ls

module Settings =

    let cliPrefix = "AoC:>"

    let commandsListHint = "Type '{0}' to get available commands list"

    let commandInfo =
        [
            Quit, ["I am too old for this sh*t"; "quit"], "Quit desc"
            Help, ["help"], "Help desc"
            About, ["wat"], "ab desc"
            Run, ["run"], "Run desc"
            Cd, ["cd"], "Cd desc"
            Ls, ["ls"], "Ls desc"
        ]
    let private superStrongDecryptionMethod = id

    let private decrypt = superStrongDecryptionMethod

    let private resourceMessages =
        [
            Intro, ["Intro Line 1"; "Intro Line 2"; "Intro line3"]
            Welcome, ["Intro Line 1"; "Intro Line 2"; "Intro line3"] //[ "Ho! Ho! Ho!"; "Welcome to Advent of Code puzzle solutions Christmas Line Interface"; commandsListHint]
            Goodbye, ["Hasta la vista, baby"]
            MissedCommand, [ "Missed command" ]
            UnknownCommand, [ "Unknown command" ]
            UnsupportedArgument, ["UnsupportedArgument"]//[ "look at me motry" ]
            NotFoundValidation, ["puzzle not found"]//[ "Looks like stolen but els claims the answer was 42" ]
            Exception, [ "exception" ]
            PastValidation, [ "in past" ]
            FutureValidation, [ "in future" ]
            RepeatUnknown 3, ["repeat"]
            RepeatCommand ("wat", 3), [ "say what one more time" ]
        ]
        |> List.map (fun (k, v) -> k, decrypt v)
        |> Map

    let messagesFor messageType=
        match resourceMessages.TryFind messageType with
        | Some messages -> messages
        | None -> []
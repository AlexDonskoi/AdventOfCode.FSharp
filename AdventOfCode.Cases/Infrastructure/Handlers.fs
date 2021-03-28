namespace AdventOfCode.Cases.Infrastructure

module Handlers =
(*
    let processQuit currentContext = function
        | [] -> Null, Settings.resourceMessages.TryFind Goodbye
        | _ -> currentContext, Settings.resourceMessages.TryFind UnsupportedArgument

    let processCd currentContext args =
        let commandContext = filterContext args
        | [] -> Null, Settings.resourceMessages.TryFind Goodbye
        | _ -> context, Settings.resourceMessages.TryFind UnsupportedArgument

    let processHelp currentContext args =
        let commandContext = filterContext args
        | [] -> Null, Settings.resourceMessages.TryFind Goodbye
        | _ -> context, Settings.resourceMessages.TryFind UnsupportedArgument

    let processAbout currentContext args =
        let commandContext = filterContext args
        | [] -> Null, Settings.resourceMessages.TryFind Goodbye
        | _ -> context, Settings.resourceMessages.TryFind UnsupportedArgument

    let processRun currentContext args =
        let commandContext = filterContext args
        | [] -> Null, Settings.resourceMessages.TryFind Goodbye
        | _ -> context, Settings.resourceMessages.TryFind UnsupportedArgument

    let processLs currentContext args =
        let commandContext = filterContext args
        | [] -> Null, Settings.resourceMessages.TryFind Goodbye
        | _ -> context, Settings.resourceMessages.TryFind UnsupportedArgument *)

    let handleCommand = function
        (*| Quit -> processQuit
        | Cd -> processCd
        | Help -> processHelp
        | About -> processAbout
        | Run -> processRun
        | Ls -> processLs*)
        | _ -> None


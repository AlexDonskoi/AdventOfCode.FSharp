namespace AdventOfCode.Cases.Infrastructure

open System

type FilterContext =
        | Null
        | Root
        | Year of int
        | Day of int * int
        | Case of int * int * Case

type ExecutionContext = FilterContext * list<Puzzle> * list<Command>

type ExecutionResult =
    | Message of list<string>

module Commands =
    let cliPrefix = "AoC:>"

    let aliases =
        Settings.commandInfo
        |> List.collect
               (fun (key, aliases, _) -> List.map (fun al -> al, key) aliases)
        |> Map

    let (|CaseX|_|): (string -> option<Case>) = function
        | "a" | "A" -> Some Case.A
        | "b" | "B" -> Some Case.B
        | _ -> None

    let applyPath currentFilter path =
        let apply = function
            | Root, Parser.Int year -> Year year |> Some
            | Year _, ".." -> Root |> Some
            | Year y, Parser.Int d -> (y, d) |> Day |> Some
            | Day (year, _), ".." -> Year year |> Some
            | Day (year, day), CaseX c -> (year, day, c) |> Case |> Some
            | Case (year, day, _), ".." -> (year, day) |> Day |> Some
            | _, _-> None

        match currentFilter with
        | Some filter -> apply (filter,path)
        | _ -> None

    let executionContext (currentFilter, puzzle, commands) = function
        | [(arg:string) ] ->
            arg.Split("/")
            |> Array.fold applyPath (Some currentFilter)
            |> function
                | Some filter -> Some (filter, puzzle, commands)
                | _ -> None
        | _ -> None

    let execute (context:ExecutionContext) = function
        | [||] -> context, Settings.messagesFor MissedCommand
        | [|cmd::args|] ->
            match aliases.TryFind cmd with
            | Some command ->
                match executionContext context args with
                //| Some context -> Handlers.handleCommand command context
                | _ -> context, Settings.messagesFor MissedCommand
            | None -> context, Settings.messagesFor UnknownCommand
        | _ -> context, Settings.messagesFor UnknownCommand

    let init = ExecutionContext (Root, Storage.puzzles , list.Empty)



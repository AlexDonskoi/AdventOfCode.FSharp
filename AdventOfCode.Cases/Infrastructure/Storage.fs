namespace AdventOfCode.Cases.Infrastructure

open System
open System.Reflection
open FileReader

type Invocation = Case -> DataSource -> obj
type Puzzle = (int * int * Invocation)

module Storage =

    let private assignable (tgt:Type) (src:Type) = tgt.IsAssignableFrom(src)

    let private invocation (method:MethodInfo) acceptType (case:Case) (source:DataSource) =
        let input =
            match source acceptType with
            | ContentType.Line x -> x :> obj
            | ContentType.Char x -> x :> obj
            | ContentType.All x -> x :> obj
            | ContentType.Group x -> x :> obj
        method.Invoke(null, [| case; input |])

    let private unsupported message _ _ :obj = message

    let internal build (method:MethodInfo) (info:PuzzleAttribute) :Puzzle =
        let invoker =
            match method.GetParameters() |> Array.map (fun t -> t.ParameterType) with
            | [| case; source |] when assignable case typeof<Case> ->
                let methodInvocation = invocation method
                match source with
                | t when t = typeof<seq<string>> -> methodInvocation AcceptType.Line
                | t when t = typeof<seq<char>> -> methodInvocation AcceptType.Char
                | t when t = typeof<string> -> methodInvocation AcceptType.All
                | t when t = typeof<seq<list<string>>> -> methodInvocation AcceptType.Group
                | _ -> unsupported "unsupported input type"
            | _ -> unsupported "unsupported method signature"
        (info.Year, info.Day, invoker)

    let internal scan (method:MethodInfo) =
        Seq.map
        <| build method
        <| method.GetCustomAttributes<PuzzleAttribute>()

    let puzzles =
        typeof<PuzzleAttribute>.Assembly.GetTypes()
        |> Array.collect (fun m -> m.GetMethods())
        |> Seq.collect scan
        |> Seq.toList

    let get year day case =
        let puzzle = puzzles |> List.tryFind (fun (y, d, _) -> y = year && d = day)
        match puzzle with
        | Some (_, _, inv) -> inv case
        | _ -> failwith "puzzle not found"



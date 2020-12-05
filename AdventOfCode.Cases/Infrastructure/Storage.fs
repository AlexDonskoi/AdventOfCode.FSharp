namespace AdventOfCode.Cases.Infrastructure

open System
open System.Reflection

type Source = seq<string>
type Invocation = Case -> Source -> obj
type Puzzle = (int * int * Invocation)

module Storage =
    let private assignable (tgt:Type) (src:Type) = tgt.IsAssignableFrom(src)

    let private invocation (method:MethodInfo) (case:Case) (source:Source) =
        method.Invoke(null, [| case; source |])

    let private fail message _ _ :obj = message

    let internal build (method:MethodInfo) (info:PuzzleAttribute) :Puzzle =
        let invoker =
            match method.GetParameters() |> Array.map (fun t -> t.ParameterType) with
            | [| case; source |] when assignable case typeof<Case> ->
                match source with
                | t when assignable t typeof<Source> -> invocation method
                | _ -> fail "unsupported input type"
            | _ -> fail "unsupported method signature"
        (info.Year, info.Day, invoker)

    let internal scan (method:MethodInfo) =
        Seq.map
        <| build method
        <| method.GetCustomAttributes<PuzzleAttribute>()

    let all (assembly:Assembly) =
        assembly.GetTypes()
        |> Array.collect (fun m -> m.GetMethods())
        |> Seq.collect scan
        |> Seq.toList

    let get year day case (items:list<Puzzle>) (src: Source)=
        let puzzle = items |> List.tryFind (fun (y, d, _) -> y = year && d = day)
        match puzzle with
        | Some (_, _, inv) -> inv case src
        | _ -> obj()



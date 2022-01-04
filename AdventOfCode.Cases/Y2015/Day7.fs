module AdventOfCode.Cases.Y2015.Day7

open System
open AdventOfCode.Cases.Utilities
open AdventOfCode.Cases.Infrastructure.Parser
open AdventOfCode.Cases.Infrastructure

type Operation =
    | Unary of (uint16->uint16)
    | Binary of string*(uint16->uint16->uint16)

type Source = | Constant of uint16 | Operation of Operation



let maxValue = 65535us
let parseUnary operation =
    match operation with
    | "NOT" ->
        function
        | UInt16 v -> Constant v
        | _ -> ((-) maxValue) |> Unary |> Operation
    | _ -> failwith "unsupported operation"

let parseBinary operation arg1 arg2 =
    match operation with
    | "NOT" ->
        function
        | UInt16 v -> Constant v
        | _ -> ((-) maxValue) |> Unary |> Operation
    | _ -> failwith "unsupported operation"

let addConnection key (tgt,op) = Map.change key (function | None -> Map [tgt, op] |> Some | Some arr -> Map.add tgt op arr |> Some)

let applyArg arg func = func arg

let foldWires (wires,connections)  = String.split " -> " >> function
    | [| src; tgt |] ->
        match String.split " " src with
        | [| UInt16 v |] ->
            let wires = Map.add tgt v wires
            wires,connections
        | [| wire |] ->
            let connections = addConnection wire (tgt, Unary id) connections
            wires,connections
        | [| "NOT"; UInt16 arg |] ->
            let wires = Map.add tgt (maxValue - arg) wires
            wires,connections
        | [| "NOT"; wire |] ->
            let connections = addConnection wire (tgt, (-) maxValue |> Unary) connections
            wires,connections
        | [| UInt16 arg1; "OR"; UInt16 arg2 |] ->
            let wires = Map.add tgt (arg1 ||| arg2) wires
            wires,connections
        | [| UInt16 arg1; "OR"; arg2 |] ->
            let connections = addConnection arg2 (tgt, (|||) arg1 |> Unary) connections
            wires,connections
        | [| arg1; "OR"; UInt16 arg2 |] ->
            let connections = addConnection arg1 (tgt, (|||) arg2 |> Unary) connections
            wires,connections
        | [| arg1; "OR"; arg2 |] ->
            let connections =
                addConnection arg1 (tgt, Binary(arg2,(|||))) connections
                |> addConnection arg2 (tgt, Binary(arg1,(|||)))
            wires,connections
        | [| UInt16 arg1; "AND"; UInt16 arg2 |] ->
            let wires = Map.add tgt (arg1 ||| arg2) wires
            wires,connections
        | [| UInt16 arg1; "AND"; arg2 |] ->
            let connections = addConnection arg2 (tgt, (&&&) arg1 |> Unary) connections
            wires,connections
        | [| arg1; "AND"; UInt16 arg2 |] ->
            let connections = addConnection arg1 (tgt, (&&&) arg2 |> Unary) connections
            wires,connections
        | [| arg1; "AND"; arg2 |] ->
            let connections =
                addConnection arg1 (tgt, Binary(arg2,(&&&))) connections
                |> addConnection arg2 (tgt, Binary(arg1,(&&&)))
            wires,connections
        | [| UInt16 arg1; "LSHIFT"; Int arg2 |] ->
            let wires = Map.add tgt (arg1 <<< arg2) wires
            wires,connections
        | [| arg1; "LSHIFT"; Int arg2 |] ->
            let connections = addConnection arg1 (tgt, (<<<) >> applyArg arg2 |> Unary) connections
            wires,connections
        | [| UInt16 arg1; "RSHIFT"; Int arg2 |] ->
            let wires = Map.add tgt (arg1 >>> arg2) wires
            wires,connections
        | [| arg1; "RSHIFT"; Int arg2 |] ->
            let connections = addConnection arg1 (tgt, (>>>) >> applyArg arg2 |> Unary) connections
            wires,connections
        | _ -> failwith "unsupported command"
    | _ -> failwith "unable to parse line"

let rec calculate wires connections =
    if Map.isEmpty connections then wires
    else
        match Map.tryPick (fun k v -> if Map.containsKey k wires then Some (k,v) else None) connections with
        | None -> failwith "calculation stuck"
        | Some (wire,wireConnections) ->
            let wireValue = Map.find wire wires
            let folder value (wires,connections) tgt op =
                match op with
                | Unary op ->
                    let wires = Map.add tgt <| op value <| wires
                    wires, connections
                | Binary (arg,op) ->
                    let connections = Map.find arg connections |> Map.add tgt (op value |> Unary) |> Map.add arg <| connections
                    wires, connections
            let wires, connections = Map.fold (folder wireValue) (wires,connections) wireConnections
            let connections = Map.remove wire connections
            calculate wires connections


let init = Seq.fold foldWires (Map.empty, Map.empty)

[<Puzzle(2015, 7)>]
let puzzle case (input:seq<string>) =
    let init = init input
    let aSignal = init ||> calculate |> Map.find "a"
    match case with
        | A -> aSignal
        | B ->
            let wires, connections = init
            let wires = Map.add "b" aSignal wires
            calculate wires connections |> Map.find "a"
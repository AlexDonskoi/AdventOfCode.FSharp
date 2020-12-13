namespace AdventOfCode.Cases.Y2020
open System
open AdventOfCode.Cases.Infrastructure

module Day12 =

    type Direction = | North | South | East | West
    type Position = (int*int*Direction)
    type Command =
        | N of int
        | S of int
        | E of int
        | W of int
        | L of int
        | R of int
        | F of int
        | I of int//ignore

    let parseLine (input:string) =
        match input.Substring(1) with
        | Parser.Int value ->
            let cmd = input.Substring(0,1) |> char
            value
            |>
            match cmd with
            | 'N' -> N
            | 'S' -> S
            | 'W' -> W
            | 'E' -> E
            | 'L' -> L
            | 'R' -> R
            | 'F' -> F
            | _ -> I
        | _-> I 0

    let rec turnLeft count dir =
        match dir with
        | North when count > 0 -> West |> turnLeft (count - 1)
        | West when count > 0 -> South |> turnLeft (count - 1)
        | South when count > 0 -> East |> turnLeft (count - 1)
        | East when count > 0 -> North |> turnLeft (count - 1)
        | d -> d
    let rec turnRight count dir =
        match dir with
        | North when count > 0 -> East |> turnRight (count - 1)
        | West when count > 0 -> North |> turnRight (count - 1)
        | South when count > 0 -> West |> turnRight (count - 1)
        | East when count > 0 -> South |> turnRight (count - 1)
        | d -> d

    let move (x, y, dir) cmd =
        let turns value = (value / 90) % 4

        match cmd, dir with
        | N v, _
        | F v, North -> (x, y + v, dir)
        | S v, _
        | F v, South -> (x, y - v, dir)
        | E v, _
        | F v, East -> (x + v, y, dir)
        | W v, _
        | F v, West -> (x - v, y, dir)
        | R v, _ -> (x, y, turnRight (turns v) dir )
        | L v, _ -> (x, y, turnLeft (turns v) dir )
        | _ -> (x, y, dir)

    let moveWaypoint ((x, y), (wpX, wpY)) cmd =
        let turns value = (value / 90) % 4
        let rec wpTurnLeft cnt (wpX, wpY) = if cnt > 0 then wpTurnLeft (cnt - 1) (-wpY, wpX) else (wpX, wpY)
        let rec wpTurnRight cnt (wpX, wpY) = if cnt > 0 then wpTurnRight (cnt - 1) (wpY, -wpX) else (wpX, wpY)

        match cmd with
        | N v -> ((x, y), (wpX, wpY + v))
        | S v -> ((x, y), (wpX, wpY - v))
        | E v -> ((x, y), (wpX + v, wpY))
        | W v -> ((x, y), (wpX - v, wpY))
        | F v -> ((x + v*wpX, y + v*wpY), (wpX, wpY))
        | R v -> ((x, y), wpTurnRight (turns v) (wpX, wpY) )
        | L v -> ((x, y), wpTurnLeft (turns v) (wpX, wpY) )
        | _ -> ((x, y), (wpX, wpY))

    let rec caseA pos = function
        | h::rest ->
            let newPos = move pos h
            caseA newPos rest
        | _ -> pos

    let rec caseB pos = function
        | cmd::rest ->
            let newPos = moveWaypoint pos cmd
            caseB newPos rest
        | _ -> pos

    [<Puzzle(2020, 12)>]
    let puzzle case (input:seq<string>) =
        let commands =
            input
            |> Seq.map parseLine
            |> Seq.toList
        match case with
        | Case.A ->
            let (x, y, _) = caseA (0, 0, East) commands
            Math.Abs(x) + Math.Abs(y)
        | Case.B ->
            let ((x, y), _) = caseB ((0, 0), (10, 1)) commands
            Math.Abs(x) + Math.Abs(y)
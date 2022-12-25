module AdventOfCode.Cases.Y2022.Day19
open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core


let sourceRegex = Regex "Blueprint (?<num>\d+): Each ore robot costs (?<ore>\d+) ore\. Each clay robot costs (?<clay>\d+) ore\. Each obsidian robot costs (?<obsidianOre>\d+) ore and (?<obsidianClay>\d+) clay\. Each geode robot costs (?<geodeOre>\d+) ore and (?<geodeObsidian>\d+) obsidian\."

let parse input =
    let getVal src name =
        match Regex.captures src name with
        | [Int v] -> v
        | _ -> failwith "incorrect source"
    let getVal = sourceRegex.Match input |> getVal
    getVal "num", ((getVal "ore"), (getVal "clay"), (getVal "obsidianOre"), (getVal "obsidianClay"), (getVal "geodeOre"), (getVal "geodeObsidian"))

let next blueprint state =
    let costOre, costClay, costObsOre, costObsClay, costGeoOre, costGeoObs = blueprint
    let robots, sources = state
    let robOre, robClay, robObs, robGeo = robots
    let ore, clay, obs, geo = sources
    let maxOre = ore/costOre
    let maxClay = ore/costClay
    let maxObs = min (ore / costObsOre) (clay / costObsClay)
    let maxGeo = min (ore / costGeoOre) (obs / costGeoObs)
    [
        if maxGeo > 0 then
            let robots = (robOre), (robClay), (robObs), (robGeo + 1)
            let sources = (ore - costGeoOre + robOre), (clay + robClay), (obs - costGeoObs + robObs),(geo + robGeo)
            yield robots, sources
        else
            if maxObs > 0 then
                let robots = (robOre), (robClay), (robObs + 1), (robGeo)
                let sources = (ore - costObsOre + robOre), (clay - costObsClay + robClay), (obs + robObs),(geo + robGeo)
                yield robots, sources
            // if maxOre > 0 then
            //     let robots = (robOre + 1), (robClay), (robObs), (robGeo)
            //     let sources = (ore - costOre + robOre), (clay + robClay), (obs + robObs),(geo + robGeo)
            //     yield robots, sources

            if maxClay > 0 then
                let robots = (robOre), (robClay + 1), (robObs), (robGeo)
                let sources = (ore - costClay + robOre), (clay + robClay), (obs + robObs),(geo + robGeo)
                yield robots, sources
            let needOre = (robClay *costObsOre + 1 >= robOre*costObsClay)
            let needOre = needOre || (robObs*costGeoOre >= robOre*costGeoObs)
            let needOre = needOre || (robClay*costObsClay*costGeoOre >= robOre*costGeoObs*costObsOre)
            let needOre = needOre || robOre < 5
            if maxOre > 0 && needOre then
                let robots = (robOre + 1), (robClay), (robObs), (robGeo)
                let sources = (ore - costOre + robOre), (clay + robClay), (obs + robObs),(geo + robGeo)
                yield robots, sources
            let robots = (robOre), (robClay), (robObs), (robGeo)
            let sources = (ore + robOre), (clay + robClay), (obs + robObs),(geo + robGeo)
            yield robots, sources
    ]

let rec count blueprint states = function
    | 0 -> states
    | iter ->
        let nextStates = Seq.collect (next blueprint) states |> Seq.toList |> List.distinct
        let maxGeo = nextStates |> List.map (fun ((_, _, _, v),_) -> v) |> List.max |> (-) <| 5
        let nextStates = nextStates |> List.filter (fun ((_, _, _, v),_) -> v > maxGeo)
        count blueprint nextStates (iter - 1)

let maxCount blueprint iter =
    let state = [(1, 0, 0, 0),(0, 0, 0, 0)]
    count blueprint state iter
    |> Seq.map (fun (_,(_, _, _, g)) -> g)
    |> Seq.max

let rec noop = function
    | 0L -> 0
    | iter -> noop (iter - 1L)

[<Puzzle(2022, 19)>]
let puzzle case (source:seq<string>) =
    let v = noop (2147483647L * 5L)


    let blueprints = Seq.map parse source |> Seq.toList
    blueprints
    |>
    match case with
    | Case.A ->
        List.map (fun (ind,b) -> maxCount b 24 |> (*) ind)
        >> List.reduce (+)
    | Case.B ->
        List.take 3
        >> List.map (fun (_,b) -> maxCount b 32)
        >> List.fold (*) 1
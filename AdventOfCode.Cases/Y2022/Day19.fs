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
    ((getVal "ore"), (getVal "clay"), (getVal "obsidianOre"), (getVal "obsidianClay"), (getVal "geodeOre"), (getVal "geodeObsidian"))

let rec next blueprint step strategy state =
    if step = 0 then Some state
    else
    let flag = strategy % 5L
    let buildGeo = flag = 1L
    let buildObs = flag = 2L
    let buildClay = flag = 3L
    let buildOre = flag = 4L
    let costOre, costClay, costObsOre, costObsClay, costGeoOre, costGeoObs = blueprint
    let robots, sources = state
    let robOre, robClay, robObs, robGeo = robots
    let ore, clay, obs, geo = sources

    if buildGeo && (costGeoOre > ore || costGeoObs > obs) then None
    else if buildObs && (costObsOre > ore || costObsClay > clay) then None
    else if buildClay && (costClay > ore) then None
    else if buildOre && (costOre > ore) then None
    else

        let ore = ore + robOre - (if buildOre then costOre else 0) - (if buildClay then costClay else 0) - (if buildObs then costObsOre else 0)  - (if buildGeo then costGeoOre else 0)
        let clay = clay + robClay - (if buildObs then costObsClay else 0)
        let obs = obs + robObs - (if buildGeo then costGeoObs else 0)
        let geo = geo + robGeo
        let robOre = robOre + (if buildOre then 1 else 0)
        let robClay = robClay + (if buildClay then 1 else 0)
        let robObs = robObs + (if buildObs then 1 else 0)
        let robGeo = robGeo + (if buildGeo then 1 else 0)
        let state = (ore, clay, obs, geo), (robOre, robClay, robObs, robGeo)
        next blueprint (step - 1) (strategy/5L) state

let rec searchStrategy blueprint steps strategy total =
    let state = (1, 0, 0, 0),(0, 0, 0, 0)
    let current =
        match next blueprint steps strategy state with
        | Some state ->
            let (_, _, _, geo), (_, _, _, robGeo) = state
            geo + robGeo
        | None -> 0
    let total = max current total
    if strategy > 0L then
       searchStrategy blueprint steps (strategy - 1L) total
    else
        total

[<Puzzle(2022, 19)>]
let puzzle case (source:seq<string>) =
    let blueprints = Seq.map parse source |> Seq.toList

    let cycles, filter, reduce =
        match case with
        | Case.A ->
            13, id, List.mapi (fun i v -> (i + 1) * v) >>  List.reduce (+)
        | Case.B ->
            let take = max 3 <| List.length blueprints
            32, List.take take, List.fold (*) 1
    let cycles = cycles - 1
    let max = pown 5L cycles - 1L

    let maxCount blueprint = searchStrategy blueprint cycles max 0
    let blueprints = filter blueprints
    blueprints
    |> List.map maxCount
    |> reduce
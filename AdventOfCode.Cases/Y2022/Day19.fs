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
    let maxClay = clay/costClay
    let maxObs = min (ore / costObsOre) (clay / costObsClay)
    let maxGeo = min (ore / costGeoOre) (obs / costGeoObs)

    seq {
        for i in 0..maxOre do
            for j in 0..maxClay do
                for k in 0..maxObs do
                    for l in 0..maxGeo do
                        let needOre = i*costOre + j*costClay + k * costObsOre + l*costGeoOre
                        let needClay = k*costObsClay + l*costGeoObs
                        let needObs = l*costGeoObs
                        if needOre > ore then ()
                        else if needClay > clay then ()
                        else
                            let robots = (robOre + i), (robClay + j), (robObs + k), (robGeo + l)
                            let sources = (ore - needOre + robOre), (clay - needClay + robClay), (obs - needObs + robObs),(geo + robGeo)
                            yield robots, sources
    }

let rec count blueprint states = function
    | 0 -> states
    | iter ->
        let nextStates = Seq.collect (next blueprint) states
        count blueprint nextStates (iter - 1)


[<Puzzle(2022, 19)>]
let puzzle case (source:seq<string>) =
    let blueprints = Seq.map parse source
    match case with
    | Case.A ->
        let state = seq { (1, 0, 0, 0),(0, 0, 0, 0) }
        blueprints
        |> Seq.collect (fun b -> count (snd b) state 24)
        |> Seq.map snd
        |> Seq.map (fun (_, _, _, g) -> g)
        |> Seq.max
    | Case.B -> 0
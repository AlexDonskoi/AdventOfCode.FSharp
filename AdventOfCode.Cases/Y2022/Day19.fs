module AdventOfCode.Cases.Y2022.Day19
open System
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

let rec isCovered key1 key2 =
    let (ore1, clay1, obs1, robOre1, robClay1, robObs1) = key1
    let (ore2, clay2, obs2, robOre2, robClay2, robObs2) = key2

    ore1 >= ore2 && clay1 >= clay2 && obs1 >= obs2 && robOre1 >= robOre2 && robClay1 >= robClay2 && robObs1 >= robObs2

let rec tryAdd key value map =
    match Map.tryFindKey (fun k v -> isCovered k key && v >= value) map with
    | Some _ -> map
    | None ->
        Map.filter (fun k v -> isCovered key k && value >= v) map
        |> Map.fold (fun acc k v -> Map.remove k acc) map
        |>Map.change key (Option.defaultValue value >> max value >> Some)

let rec search (maxOre, maxClay, maxObs, costOre, costClay, costObsOre, costObsClay, costGeoOre, costGeoObs) states remain =
    let mutable newMap = Map.empty

    if remain = 0 then Map.values states |> Seq.max
    else

    for k in Map.keys states do
        let earn = Map.find k states
        let (core, cclay, cobs, robOre, robClay, robObs) = k
        let ore = if robOre >= maxOre then maxOre else (core + robOre)
        let clay = if robClay >= maxClay then maxClay else (cclay + robClay)
        let obs = if robObs >= maxObs then maxObs else (cobs + robObs)
        let needOre = robOre < maxOre //&& (remain*maxOre > (remain - 1)*robOre)
        let needClay = robClay < maxClay //&& (remain*maxClay > (remain - 1)*robClay)
        let needObs = robObs < maxObs //&& (remain*maxObs > (remain - 1)*robObs)
        //let badWay = (remain < 4 && earn = 0) || (remain < 12 && robObs = 0) || (remain < 18 && robClay = 0)
        //if badWay then states
        //else
        newMap<-tryAdd (ore, clay, obs, robOre, robClay, robObs) earn newMap
        newMap<-
            if needOre && costOre <= core
            then
                let ore = if robOre >= maxOre then maxOre else (core - costOre + robOre)
                tryAdd (ore, clay, obs, robOre + 1, robClay, robObs) earn newMap
            else newMap
        newMap<-
            if needClay && costClay <= core
            then
                let ore = if robClay >= maxClay then maxOre else (core - costClay + robOre)
                tryAdd (ore, clay, obs, robOre, robClay + 1, robObs) earn newMap
            else newMap
        newMap<-
            if needObs && costObsOre <= core && costObsClay <= cclay
            then
                let ore = if robOre >= maxOre then maxOre else (core - costObsOre + robOre)
                let clay = if robClay >= maxClay then maxClay else (cclay - costObsClay + robClay)
                tryAdd (ore, clay, obs, robOre, robClay, robObs + 1) earn newMap
            else newMap
        newMap<-
            if costGeoOre <= core && costGeoObs <= cobs
            then
                let ore = if robOre >= maxOre then maxOre else (core - costGeoOre + robOre)
                let obs = if robObs >= maxObs then maxObs else (cobs - costGeoObs + robObs)

                tryAdd (ore, clay, obs, robOre, robClay, robObs) (earn + remain - 1) newMap
            else newMap
    search (maxOre, maxClay, maxObs, costOre, costClay, costObsOre, costObsClay, costGeoOre, costGeoObs) newMap (remain - 1)

let run (costOre, costClay, costObsOre, costObsClay, costGeoOre, costGeoObs) =
    let maxOre = max costOre costClay |> max costObsOre |> max costGeoOre
    search (maxOre, costObsClay, costGeoObs, costOre, costClay, costObsOre, costObsClay, costGeoOre, costGeoObs)


[<Puzzle(2022, 19)>]
let puzzle case (source:seq<string>) =
    let blueprints = Seq.map parse source |> Seq.toList

    let cycles, filter, reduce =
        match case with
        | Case.A ->
            24, id, List.mapi (fun i v -> (i + 1) * v) >>  List.reduce (+)
        | Case.B ->
            let take = min 3 <| List.length blueprints
            32, List.take take, List.fold (*) 1

    let initState = Map.empty |> Map.add (0, 0, 0, 1, 0, 0) 0
    filter blueprints
    |> List.map (fun b -> run b initState cycles)
    |> reduce

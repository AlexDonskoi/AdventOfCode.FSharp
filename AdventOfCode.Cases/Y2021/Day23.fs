module AdventOfCode.Cases.Y2021.Day23

open AdventOfCode.Cases.Infrastructure
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core

type Amphipod = | A | B | C | D
type Room = list<Amphipod>
type Hall = Map<int,Amphipod>
type State = Room*Room*Room*Room*Hall*int

let caseAState = [D;D],[B;A],[C;B],[C;A]
let caseBState = [D;D;D;D],[B;C;B;A],[C;B;A;B],[C;A;C;A]

let reachable ifrom ito map =
    if ifrom > ito then [ito..ifrom-1] else [ifrom+1..ito]
    |> List.fold (fun acc cur -> Map.containsKey cur map |> not && acc) true

let moveCost mv =
    function
    | A -> 1
    | B -> 10
    | C -> 100
    | D -> 1000
    >> (*) mv
let rec roomCost room owner =
    let outcom = List.mapi (fun i v -> moveCost (i+1) v) room |> List.sum
    let size = List.length room
    let income = moveCost <| size *(size + 1)/2 <| owner
    outcom + income

let rec initCost rooms =
    let roomA,roomB,roomC,roomD = rooms
    roomCost roomA A
    |> (+) (roomCost roomB B)
    |> (+) (roomCost roomC C)
    |> (+) (roomCost roomD D)


//12 4 6 8 1011
//  3 5 7 9 = hall places

let index = function | A -> 3 | B -> 5 | C -> 7 | D -> 9

let room el state =
    let roomA,roomB,roomC,roomD,_,_ = state
    match el with | A -> roomA | B -> roomB | C -> roomC | D -> roomD


let normalizeHall continuation state =
    let pick hall k v =
        let ind = index v
        let reachable = reachable k ind hall
        let emptyTarget = room v state |> List.isEmpty
        if reachable && emptyTarget then
            let cost = moveCost <| abs (k - ind) <| v
            Some (k,cost)
        else None
    let roomA,roomB,roomC,roomD,hall,cost = state
    match Map.tryPick (pick hall) hall with
    | Some (k,c) ->
        let hall = Map.remove k hall
        continuation (roomA,roomB,roomC,roomD,hall,(cost+c))
    | _ -> state


let normalizeRoom state owner =
    let _,_,_,_,hall,_ = state

    match room owner state with
    | [] -> None
    | h::_ when h = owner -> None
    | h::rest ->
        let toInd = index h
        let fromInd = index owner
        let reachable = reachable fromInd toInd hall
        let emptyTarget = room h state |> List.isEmpty
        if reachable && emptyTarget then
            let c = moveCost <| abs (toInd - fromInd) <| h
            Some (rest,c)
        else None

let rec normalize state =
    let roomA,roomB,roomC,roomD,hall,cost = state
    match normalizeRoom state A with
    | Some (rest,c) when c > 0 -> normalize (rest,roomB,roomC,roomD,hall,(cost + c))
    | _ ->
        match normalizeRoom state B with
        | Some (rest,c)  when c > 0 -> normalize (roomA,rest,roomC,roomD,hall,(cost + c))
        | _ ->
            match normalizeRoom state C with
            | Some (rest,c)  when c > 0 -> normalize (roomA,roomB,rest,roomD,hall,(cost + c))
            | _ ->
                match normalizeRoom state D with
                | Some (rest,c)  when c > 0 -> normalize (roomA,roomB,roomC,rest,hall,(cost + c))
                | _ -> normalizeHall normalize state

let roomNext room owner hall =
    let ind = index owner
    [ match room with
        | h::rest ->
            yield! [for i in [1;2;4;6;8;10;11] do
                     if reachable ind i hall then
                        let hall = Map.add i h hall
                        let cost = moveCost <| abs (ind - i) <| h
                        yield rest,hall,cost
                     else ()]
        | _ -> ()]

let next (state:State):list<State> =
    let roomA,roomB,roomC,roomD,hall,cost = state
    [
        yield! [for r,h,c in roomNext roomA A hall -> r,    roomB,roomC,roomD,h,(cost + c)]
        yield! [for r,h,c in roomNext roomB B hall -> roomA,r,    roomC,roomD,h,(cost + c)]
        yield! [for r,h,c in roomNext roomC C hall -> roomA,roomB,r,    roomD,h,(cost + c)]
        yield! [for r,h,c in roomNext roomD D hall -> roomA,roomB,roomC,r,    h,(cost + c)]
    ]

let isCompleted state =
    let roomA,roomB,roomC,roomD,hall,cost = state
    List.isEmpty roomA && List.isEmpty roomB && List.isEmpty roomC && List.isEmpty roomD && Map.isEmpty hall

let rec run acc states =
    let completed, rest = List.map normalize states |> List.partition isCompleted
    let acc = List.append completed acc
    match rest with
    | [] -> acc
    | _ ->
        let states = List.collect next rest
        run acc states

let clear room owner = List.rev room |> List.skipWhile ((=) owner) |> List.rev

let exec rooms =
    let roomA,roomB,roomC,roomD = rooms
    let roomA,roomB,roomC,roomD = (clear roomA A,clear roomB B,clear roomC C,clear roomD D)
    let cost = initCost (roomA,roomB,roomC,roomD)
    let state = (roomA,roomB,roomC,roomD,Map.empty,cost) |> State
    run [] [state]
    |> List.map (fun (_,_,_,_,_,c) -> c)
    |> List.min

[<Puzzle(2021, 23)>]
let puzzle case (input:string) =
    input |>ignore
    match case with
    | Case.A -> caseAState
    | Case.B -> caseBState
    |> exec



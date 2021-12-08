module AdventOfCode.Cases.Y2021.Day7

open AdventOfCode.Cases.Infrastructure

let costB distance = (distance * distance + distance) /2L
let costA distance = distance
let costAll cost pos = (-) <| pos >> abs >> cost |> List.sumBy

let tupleMap3 f (x,y,z) = f x, f y, f z

(*
    In assumption cost function have single local minimum. Count in 5 point and decide which half contains minimum
    use power of 2 so can continue same way with half
    start  mid      finish
    |       |       |
    |...|...|...|...|
       left   right

*)
let rec calculate cost step points distances =
    let start,mid,finish =
        match points with
        | None -> (0L, 1L, 2L) |> tupleMap3 ((*) step >> cost >> (|>) distances)
        | Some b -> b

    let midStep = step / 2L
    let left = cost midStep distances
    let right = cost (3L*midStep) distances
    let calculate = Some >> calculate cost midStep
    match midStep with
    | 1L -> List.min [start; left; mid; right; finish;]
    | _ when left < mid -> calculate (start, left, mid) distances
    | _ when right < mid ->
        let distances = List.map (fun v -> v - step) distances // adjust distances related to new starting point
        calculate (mid, right, finish) distances
    | _ ->
        let distances = List.map (fun v -> v - midStep) distances // adjust distances related to new starting point
        calculate (left, mid, right) distances

let rec step acc src =
    match src with
    | 1L -> acc
    | _ -> step <| (acc * 2L) <| (src / 2L)


[<Puzzle(2021, 7)>]
let puzzle case (input:string) =
    let cost = costAll <| match case with | Case.A -> costA | Case.B -> costB
    let src =
        input
        |> String.split ","
        |> Array.map int64
        |> Array.toList

    let step = src |> List.max |> step 1L
    calculate cost step None src

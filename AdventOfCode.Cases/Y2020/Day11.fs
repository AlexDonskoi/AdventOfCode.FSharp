namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure

module Day11 =

    type Seat = | Empty | Occupied | NotExists

    let parseItem = function
        | 'L' -> Empty
        | '#' -> Occupied
        | _ -> NotExists

    let parseLine = Seq.map parseItem >> Seq.toList

    let directions = [for x in -1..1 do
                          for y in -1..1 do
                              if x <> 0 || y <>0 then yield (fun (i, j) -> (i + x, y + j))else ()]

    let rec directionSeat stopPredicate source pos move =
        let newPos = move pos
        match Map.tryFind newPos source with
        | Some s when stopPredicate s -> s
        | Some s -> directionSeat stopPredicate source newPos move
        | _ -> NotExists

    let allDirectionSeats stopPredicate source currentPos =
        let mapper = directionSeat stopPredicate source currentPos
        List.map mapper directions

    let nextRound tolerance directionStop source:Map<(int*int),Seat> =
        let seatState key value =
            let occupiedSeats =
                allDirectionSeats directionStop source key
                |> List.filter ((=) Occupied)
                |> List.length
            match value with
            | Occupied when occupiedSeats >= tolerance -> Empty
            | Empty when occupiedSeats = 0 -> Occupied
            | seat -> seat

        let folder map key value =
            Map.add key <| seatState key value <| map

        Map.fold folder Map.empty source


    let occupiedCount (seats:Map<(int*int),Seat>) =
        seats
        |> Map.filter (fun _ v -> v = Occupied)
        |> Map.count

    let rec run tolerance directionStop prevCount seats =
        let next = nextRound tolerance directionStop seats
        let stepOccupiedCount = occupiedCount next
        if stepOccupiedCount = prevCount then stepOccupiedCount
            else run tolerance directionStop stepOccupiedCount next

    let stopAny _ = true
    let stopExisting = function
        | Occupied | Empty -> true
        | _ -> false

    [<Puzzle(2020, 11)>]
    let puzzle case (input:seq<string>) =
        input
        |> Seq.toList
        |> List.map parseLine
        |> List.mapi (fun ind -> List.mapi (fun y it -> (ind, y), it))
        |> List.collect id
        |> List.fold (fun mp (k, v) -> Map.add k v mp) Map.empty
        |> match case with
            | Case.A -> run 4 stopAny 0
            | Case.B -> run 5 stopExisting 0


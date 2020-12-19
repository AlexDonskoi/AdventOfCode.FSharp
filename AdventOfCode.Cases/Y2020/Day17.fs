namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure

module Day17 =

    type Coord = list<int>
    type State = | Active | Inactive

    type Dimension = ((int * int * int) * int * list<Coord>)

    let parseItem = function
        | '#' -> Active
        | _ -> Inactive

    let parseLine = Seq.map parseItem >> Seq.toList

    let parseDimension hyperSize input:Dimension =
        let states =
            input
            |> List.map parseLine
        let sizeY = List.length states
        let sizeX = states |> List.map List.length |> List.max

        let activeCells =
              [ for indY, line in List.mapi (fun ind el -> ind, el) states do
                  for indX, state in  List.mapi (fun ind el -> ind,el) line do
                      if state = Active then yield (List.append [indX; indY] (List.replicate hyperSize 0))
                        else ()]
        ((sizeX, sizeY, hyperSize), 0, activeCells)

    let neighbors coords =
        let folder acc cur =
            let singleDimension = [for x in -1 .. 1 -> [x + cur]]
            if List.isEmpty acc
                then singleDimension
                else
                    singleDimension
                    |> List.allPairs acc
                    |> List.map (fun (fst, snd) -> List.append fst snd)
        List.fold folder [] coords
        |> List.except [coords]

    let rec hyperCoord hyperSize cycle target =
        if hyperSize <= 0 then target
        else
            [for c in -cycle .. cycle -> c]
            |> List.allPairs target
            |> List.map (fun (fst, snd) -> List.append fst [snd])
            |> hyperCoord (hyperSize - 1) cycle

    let allCells (sizeX, sizeY, hyperSize) cycle =
        [for x in -cycle .. (sizeX + cycle - 1) do
             yield! [for y in -cycle .. (sizeY + cycle - 1) -> [x; y]]]
        |> hyperCoord hyperSize cycle

    let applyCellCycle (accActive, prevActive) cell =
        let activeNeighborsCount =
            cell
            |> neighbors
            |> List.filter (fun el -> List.contains el prevActive)
            |> List.length

        let appendActive =
            match List.contains cell prevActive with
            | true when activeNeighborsCount >= 2 && activeNeighborsCount <= 3 -> List.append accActive [cell]
            | false when activeNeighborsCount = 3 -> List.append accActive [cell]
            | _ -> accActive
        appendActive, prevActive

    let nextCycle (size, cycle, activeCells):Dimension =
        let cycle = cycle + 1
        let (cycleActiveCells, _) =
            allCells size cycle
            |> List.fold applyCellCycle ([], activeCells)

        (size, cycle, cycleActiveCells)

    let rec runCycles num target =
        if num <= 0 then target
            else
                target
                |> nextCycle
                |> runCycles (num - 1)

    let run target =
        let (_, _, state) =
            runCycles 6 target
        List.length state


    [<Puzzle(2020, 17)>]
    let puzzle case (input:seq<string>) =
        let hyperSize =
            match case with
            | Case.A -> 1
            | Case.B -> 2
        input
        |> Seq.toList
        |> parseDimension hyperSize
        |> run
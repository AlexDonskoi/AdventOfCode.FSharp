namespace AdventOfCode.Cases.Y2020

open System
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser

module Day23 =

    module Map =
        type Mapper = Map<int,int>

        let ofList = Map.ofList
        let add = Map.add
        let find = Map.find
        let change = Map.change
        let count = Map.count

    module Storage =

        type Mapper = int[]

        let init cnt src =
            let tgt = Array.init cnt (id >> (+) 1)
            [for ind in 1..cnt do
                 Array.set tgt (ind - 1) (ind + 1)] |> ignore
            [for k, v in List.sortBy fst src do
                 Array.set tgt (k - 1) v] |> ignore
            tgt

        let add _ v tgt = Array.append tgt [|v|]

        let find ind = Array.item (ind - 1)

        let change ind v tgt =
            Array.set tgt (ind - 1) v
            tgt
        let count = Array.length

    type State = int * Storage.Mapper

    let parse = Seq.map (charToInt >> int) >> Seq.toList

    let rec pick count map acc =
        match List.tryLast acc with
        | Some last when count > 0 -> map |> Storage.find last |> List.singleton |> List.append acc |> pick (count - 1) map
        | _ -> acc

    let rec destination max dest exclude =
        if dest < 1 then destination 1 max exclude
        else if max <= 0 then failwith "cannot find"
            else if List.contains dest exclude then destination max (dest - 1) exclude else dest

    let rec run moves (cur, map) =
        let max = Storage.count map |> int
        if moves <= 0 then map
        else

            let pickedCups =  map |> Storage.find cur |> List.singleton |> pick 2 map
            let destinationCup = destination max (cur - 1) pickedCups
            let head = List.head pickedCups
            let last = List.last pickedCups
            let newMap =
                map
                |> Storage.change cur (Storage.find last map)
                |> Storage.change last (Storage.find destinationCup map)
                |> Storage.change destinationCup head
            //if moves % 500000 = 0 then printfn "%d" moves else ()
            run (moves - 1) <| (Storage.find cur newMap, newMap)

    let init max input:State =
        let head = List.head input
        let map = List.append input [head] |> List.pairwise |> Storage.init max
        let srcLength = List.length input |> int
        let last = List.last input
        head,
        if srcLength >= max then map
            else map
                |> Storage.change last (srcLength + 1)
                |> Storage.change max head

    let caseA map =
        map
        |> Storage.find 1
        |> List.singleton
        |> pick (Storage.count map - 2) map
        |> List.map string
        |> String.concat ""

    let caseB map =
        map
        |> Storage.find 1
        |> List.singleton
        |> pick 1 map
        |> List.map int64
        |> List.reduce (*)
        |> string

    [<Puzzle(2020, 23)>]
    let puzzle case (input:string) =
        input
        |> parse
        |>
        match case with
        | Case.A -> init 9 >> run 100 >> caseA
        | Case.B -> init 1000000 >> run 10000000 >> caseB
namespace AdventOfCode.Cases.Y2020

open System
open AdventOfCode.Cases.Infrastructure

module Day9 =

    module LimitedFifo =

        type Storage = {
            adjustment:(int64 -> int64)
            limit:int64
            state:Option<int64>
            items:list<int64>}

        let create limit adjustment =
            { adjustment = adjustment; limit = limit; state = None; items =[] }

        let hasSpace storage =
            match storage.state with
            | Some st -> st < storage.limit
            | None -> true

        let rec clearSpace space storage =
            match storage.items with
            | _ when space <= 0L -> storage
            | [] -> storage
            | [_] -> { storage with state = None; items = []}
            | head::tail->
                let adj = storage.adjustment head
                let newState =
                    match storage.state with
                    | Some x -> Some (x  - adj)
                    | None -> None
                let newStorage = { storage with state = newState; items = tail }
                if adj >= space then newStorage
                    else clearSpace (space - adj) newStorage

        let push item storage =
            let adj = storage.adjustment item
            match storage.state with
            | Some state when adj <= storage.limit ->
                let newStorage = clearSpace (state + adj - storage.limit) storage
                let newState =
                    match newStorage.state with
                    | Some x -> x  + adj
                    | None -> adj
                { newStorage with state = Some newState; items = List.append newStorage.items [item] }
            | None when adj <= storage.limit -> { storage with state = Some adj; items = [item] }
            | _  -> create storage.limit storage.adjustment

        let rec pushAll items storage =
            match items with
            | h::tail -> push h storage |> pushAll tail
            | [] -> storage

    module CaseA =
        let checkPairSum sum (storage:LimitedFifo.Storage) =
            let items = storage.items
            if storage |> LimitedFifo.hasSpace then true
            else
                // count all pairs to math sum
                // TODO: review if it possible to archive with mor efficient way
                seq {for x in items do
                        for y in items do
                            if x <> y && x+y = sum
                                then yield true
                                else ()}
                |> Seq.contains true

        let rec searchItem source (storage:LimitedFifo.Storage) =
            match source with
            | [] -> None
            | h::tail ->
                if checkPairSum h storage
                    then LimitedFifo.push h storage |> searchItem tail
                else Some h

        let run tailSize source =
            let (start, tail) = List.splitAt tailSize source
            LimitedFifo.create (int64 tailSize) (fun _ -> 1L)
            |> LimitedFifo.pushAll start
            |> searchItem tail


    module CaseB =

        let rec searchContiguous minSize source (storage:LimitedFifo.Storage) =
            match source with
            | [] -> None
            | h::tail ->
                if LimitedFifo.hasSpace storage || List.length storage.items < minSize
                    then LimitedFifo.push h storage |> searchContiguous minSize tail
                else Some storage.items

        let rec getMinMax acc list =
            match acc, list with
            | acc, [] -> acc
            | (0L,0L), h::tail -> getMinMax (h, h) tail
            | (mn, mx), h::tail -> getMinMax (Math.Min(mn, h), Math.Max(mx, h)) tail

        let run minSize sum source =
            LimitedFifo.create sum id
            |> searchContiguous minSize source
            |> function
                | Some list ->
                    let (mn,mx) = getMinMax (0L,0L) list
                    Some (mn + mx)
                | None -> None


    [<Puzzle(2020, 9)>]
    let puzzle case (input:seq<string>) =
        let source =
            input
            |> Seq.map int64
            |> Seq.toList

        let caseAResult = CaseA.run 25 source

        match case with
            | Case.A -> caseAResult
            | Case.B ->
                match caseAResult with
                | None -> None
                | Some x -> CaseB.run 2 x source
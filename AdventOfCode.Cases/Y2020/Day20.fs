namespace AdventOfCode.Cases.Y2020

open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure

module Day20 =

    type Side = | Top | Right | Bottom | Left

    type Borders = string*string*string*string

    type Tile = int64 * Borders * list<string>

    let parseTileNum input =
        let tileRegex = Regex "\d+"
        let tileMatch = tileRegex.Match input
        if tileMatch.Success then tileMatch.Value |> int64
            else failwith $"incorrect tile {input}"

    let parseTileImage =
        List.skip 1 >> List.rev // cut first line
        >> List.skip 1 >> List.rev // cut last line
        >> List.map (Seq.skip 1 >> Seq.rev >> Seq.skip 1 >> Seq.rev >> String.joinSeq "" )

    let parseBorders input =
        let lines = String.split "\n" input
        (
            Array.head lines, // top line
            Array.map Seq.last lines |> String.joinSeq "", // right column
            Array.last lines |> Seq.rev |> String.joinSeq "", // bottom line
            Array.map Seq.head lines |> Seq.rev |> String.joinSeq "" // left column
        ),
        lines |> Array.toList |> parseTileImage

    let parseTile input:Tile =
        String.splitCount "\n" 2 input
        |>
        function
        | [| srcTileNum; srcLines |] ->
            let tileNum = parseTileNum srcTileNum
            let (borders, image) = parseBorders srcLines
            (tileNum, borders, image)
        | arr -> failwith $"incorrect block {arr}"

    let parse input =
        String.split "\n\n" input
        |> Array.map parseTile
        |> Array.toList

    let bordersList (t,r,b,l) = [t; r; b; l]

    let allBorders (borders:list<Borders>) =
        let direct =
            List.collect bordersList borders
        let reverse = direct |> List.map String.reverse
        List.append direct reverse

    let borderNeighboursCount (tiles:list<string>) (tileNum, borders, src) =
        let count src tgt = src |> List.filter ((=) tgt) |> List.length
        (tileNum,
            borders,
            src),
        borders |> bordersList |> List.map (count tiles)

    let borders (_, bord, _) = bord

    let getBorder side: Borders -> string = function
        | target, _, _, _ when side = Top ->  target
        | _, target, _, _ when side = Right ->  target
        | _, _, target, _ when side = Bottom ->  target
        | _, _, _, target when side = Left ->  target
        | brd -> failwith $"incorrect border {brd}"

    let tileNum (num, _, _) = num
    let tileImage (_, _, image) = image

    let neighboursCount (tiles:list<Tile>) =
        let mapper = tiles |> List.map borders |> allBorders |> borderNeighboursCount
        tiles
        |> List.map mapper

    let corners (tiles:list<Tile>) =
        let neighboursFilter = function
            // corner tile has to have two adjusted borders which not match any other
            // compare with 1 as they match it self (all border included in list)
            | [1;1;_;_] | [_;1;1;_] | [_;_;1;1] | [1;_;_;1] -> true
            | _ -> false

        tiles
        |> neighboursCount
        |> List.filter (snd >> neighboursFilter)

    let swipe (a, b) = (b, a)

    let imageCounterClockwise =
        let srcFolder acc (cur:seq<char>) =
            let tgt = cur |> Seq.rev |> Seq.map string
            if Seq.isEmpty acc then tgt
                else Seq.map2 (+) acc tgt
        List.fold srcFolder Seq.empty >> Seq.toList

    let imageClockwise =
        let srcFolder acc (cur:seq<char>) =
            let tgt = Seq.map string cur
            if Seq.isEmpty acc then tgt
                else Seq.map2 (+) tgt acc
        List.fold srcFolder Seq.empty >> Seq.toList

    let imageFlipY = List.map String.reverse

    let imageFlipX = List.rev

    let counterClockWise ((num, (t, r, b, l), src):Tile):Tile =
        num,
        (r, b, l, t) ,
        imageCounterClockwise src

    let clockwise ((num, (t, r, b, l), src):Tile):Tile  =
        num,
        (l, t, r, b),
        imageClockwise src

    let flipOverY ((num, (t, r, b, l), src):Tile):Tile  =
        num,
        (String.reverse t, String.reverse l, String.reverse b, String.reverse r),
        imageFlipY src

    let flipOverX ((num, (t, r, b, l), src):Tile):Tile =
        num,
        (String.reverse b, String.reverse r, String.reverse t, String.reverse l),
        imageFlipX src

    let start tiles =
        tiles |> corners |> List.head |> function
            | tile, [1;_;_;1] -> tile // top-left corner
            | tile, [_;_;1;1] -> tile |> clockwise // bottom-left corner need to turn
            | tile, [_;1;1;_] -> tile |> clockwise |> clockwise // bottom-right corner turn twice
            | tile, [1;1;_;_] -> tile |> counterClockWise // bottom-right corner turn twice
            | _ -> failwith "incorrect corner format"

    // TODO: exclude used items for optimization
    let rec rightNeighbour sourceTile tiles: option<Tile> =
        let borderCode = sourceTile |> borders |> getBorder Right
        match tiles with
        | h::rest when tileNum h = tileNum sourceTile -> rightNeighbour sourceTile rest
        | h::rest ->
            match h with
            | _, (tp, _, _, _), _ when tp = borderCode -> h |> counterClockWise |> flipOverX |> Some
            | _, (tp, _, _, _), _ when String.reverse tp = borderCode -> h |> counterClockWise |> Some
            | _, (_, rt, _, _), _ when rt = borderCode -> h |> flipOverY |> Some
            | _, (_, rt, _, _), _ when String.reverse rt = borderCode -> h |> clockwise |> clockwise |> Some
            | _, (_, _, bt, _), _ when bt = borderCode -> h |> clockwise |> flipOverX |> Some
            | _, (_, _, bt, _), _ when String.reverse bt = borderCode -> h |> clockwise |> Some
            | _, (_, _, _, lt), _ when lt = borderCode -> h |> flipOverX |> Some
            | _, (_, _, _, lt), _ when String.reverse lt = borderCode -> h |> Some
            | _ -> rightNeighbour sourceTile rest
        | [] -> None

    let rec ltrTileLine tiles tmpLine =
        let srcTile = tmpLine |> List.last
        match rightNeighbour srcTile tiles with
        | Some tile ->
            List.append tmpLine [tile]
            |> ltrTileLine tiles
        | None -> tmpLine

    let rec ltrTile2D acc tiles lineStart =
        let newAcc = ltrTileLine tiles [lineStart] |> List.singleton |> List.append acc
        let srcTile = lineStart |> counterClockWise
        match rightNeighbour srcTile tiles with
        | Some bottomTile -> bottomTile |> clockwise |> ltrTile2D newAcc tiles
        | None -> newAcc

    let reassemble tiles =
        let startTile = start tiles
        ltrTile2D [] tiles startTile

    let image tiles =
        let reducer acc cur = List.map2 (+) acc cur
        let mapper = List.map tileImage >> List.reduce reducer
        reassemble tiles
        |> List.collect mapper

    module CaseA =

        let cornerTiles tiles =
            let reassembledTiles = reassemble tiles
            let firstLine = List.head reassembledTiles
            let lastLine = List.last reassembledTiles
            [List.head firstLine; List.last firstLine; List.head lastLine; List.last lastLine]

    module CaseB =

        let monsterMidRegex = Regex "#(?=(?:[#.]{4}##){3}#)"
        let monsterBotRegex = Regex "#(?=(?:[#.]{2}#){5})"

        let partPositions input (regex:Regex) = [for m in regex.Matches(input) do
                                                     for c in m.Captures do yield c.Index]

        let monsterPos = function
            | [head; mid; bot]  ->
                let midPosition = partPositions mid monsterMidRegex
                let botPosition = partPositions bot monsterBotRegex
                let headPos = head |> Seq.mapi (fun i el -> i, el) |> Seq.filter (snd >> (=) '#') |> Seq.map fst |> Seq.toList
                [for m in midPosition do
                     for b in botPosition do
                         for h in headPos do
                             if (m + 1) = b && (h - 18) = m then yield m else ()]
            | _ -> []

        let monsterCount = List.windowed 3 >> List.collect monsterPos >> List.length

        let imageTransformations = [
            id
            imageClockwise
            imageClockwise
            imageClockwise
            imageClockwise >> imageFlipX
            imageClockwise
            imageClockwise
            imageClockwise
        ]

        let rec monsterCountRec srcImage  = function
            | [] -> 0
            | transform::rest ->
                let transformImage = transform srcImage
                let count = monsterCount transformImage
                if count > 0 then count
                    else monsterCountRec transformImage rest

        let monsterSearch tiles =
            let srcImage = image tiles
            let found =
                monsterCountRec srcImage imageTransformations
                |> (*) -15
            srcImage
            |> List.map (Seq.filter ((=) '#') >> Seq.length)
            |> List.reduce (+)
            |> (+) found


    [<Puzzle(2020, 20)>]
    let puzzle case (input:string) =
        input
        |> parse
        |>
        match case with
        | Case.A -> CaseA.cornerTiles >> List.map tileNum >> List.fold (*) 1L
        | Case.B -> CaseB.monsterSearch >> int64
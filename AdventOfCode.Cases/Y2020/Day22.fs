namespace AdventOfCode.Cases.Y2020

open AdventOfCode.Cases.Infrastructure

module Day22 =

    type Card = int

    type Deck = list<Card>

    let parseDeck:string->Deck =
        String.split "\n" >> Array.tail >> Array.toList >> List.map int

    let pair = function
        | [| h; l |] -> h,l
        | _ -> [],[]

    let parse:string->Deck*Deck =
        String.split "\n\n" >> Array.take 2 >> Array.map parseDeck >> pair

    let score = List.rev >> List.mapi ((+) 1 >> (*)) >> List.reduce (+)

    module Combat =
        let rec winner = function
                | ([], deck) -> deck
                | (deck, []) -> deck
                | (h1::rest1, h2::rest2) when h1 > h2 ->
                    winner (List.append rest1 [h1; h2], rest2)
                | (h1::rest1, h2::rest2) when h1 < h2 ->
                    winner (rest1, List.append rest2 [h2; h1])
                | _ -> failwith "something goes wrong"


    module RecursiveCombat =

        type State =
            | Init
            | Rec of State * list<Deck*Deck>

        let rec isRepeated state = function
            | h::_ when h = state -> true
            | _::rest -> isRepeated state rest
            | [] -> false

        let possibleSubGame deck1 deck2 =
            let isSatisfy deck = List.isEmpty deck |> not && List.length deck > List.head deck
            isSatisfy deck1 && isSatisfy deck2

        let rec internal winnerRec (root:State) history decks =
            let isRepeated = isRepeated decks history
            let couldStartSub =  decks ||> possibleSubGame
            let adjHistory = List.append history [decks]

            match root, decks with
            | Init, _ when isRepeated -> decks
            | Init, (_, []) -> decks
            | Init, ([], _) -> decks

            | Rec (prevRoot, prevHistory), (draw1::rest1, draw2::rest2) when isRepeated ->
                // return to the root game
                winnerRec prevRoot prevHistory (List.append rest1 [draw1; draw2], rest2)

            | prevRoot, (draw1::rest1, draw2::rest2) when couldStartSub ->  // continue recursive
                // start sub-game
                let (subDeck, _) = winnerRec Init [] (List.take draw1 rest1, List.take draw2 rest2)
                let stepDecks =
                    if List.isEmpty subDeck
                        then (rest1, List.append rest2  [draw2; draw1])
                        else (List.append rest1 [draw1; draw2] , rest2)
                winnerRec prevRoot adjHistory stepDecks

            | prevRoot, (draw1::rest1, draw2::rest2) when draw1 > draw2 ->
                // continue by comparison
                winnerRec prevRoot adjHistory (List.append rest1 [draw1; draw2] , rest2)

            | prevRoot, (draw1::rest1, draw2::rest2) when draw1 < draw2 ->
                // return continue
                winnerRec prevRoot adjHistory (rest1, List.append rest2  [draw2; draw1])

            | _ -> failwith "something goes wrong"

        let winner = winnerRec Init [] >> function
            | (tgt, _) when List.isEmpty tgt |> not -> tgt
            | (_, tgt) -> tgt

    [<Puzzle(2020, 22)>]
    let puzzle case (input:string) =
        input
        |> parse
        |>
        match case with
        | Case.A -> Combat.winner >> score
        | Case.B -> RecursiveCombat.winner >> score
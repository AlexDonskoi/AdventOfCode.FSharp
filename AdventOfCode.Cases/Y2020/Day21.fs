namespace AdventOfCode.Cases.Y2020

open System.Text.RegularExpressions
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Regex

module Day21 =

    type Allergen = string
    type Ingredient = string

    type AllergenMap = list<Allergen * Ingredient>

    type Food = list<Ingredient> * list<Allergen>

    let foodRegex = Regex "^((?<ingredient>\w+)[ ]?)+(\(contains([,]? (?<allergen>\w+))+\))?$"

    let parseFood input:Food =
        let foodMatch = foodRegex.Match input
        if foodMatch.Success then
            captures foodMatch "ingredient",
            captures foodMatch "allergen"
            else failwith $"unexpected food format {input}"

    (*let rec allergenMap (foods:list<Food>):AllergenMap =
        let allergenFolder ingredients map allergen =
            let ingredientsSet = Set.ofList ingredients
            map
            |>
            match Map.tryFind allergen map with
            | Some set ->
                Set.intersect set ingredientsSet
                |> Map.add allergen
            | _ -> Map.add allergen ingredientsSet

        let foodFolder map (ingredients, allergens) =
            List.fold (allergenFolder ingredients) map allergens
        List.fold foodFolder Map.empty foods*)

    let allergenMap (foods:list<Food>):AllergenMap =

        // filter only intersected elements for allergen
        let intersectionItemFolder ingredients map allergen =
            let ingredientsSet = Set.ofList ingredients
            map
            |>
            match Map.tryFind allergen map with
            | Some set ->
                Set.intersect set ingredientsSet
                |> Map.add allergen
            | _ -> Map.add allergen ingredientsSet

        // build pair where key is allergen and value is all ingredient where is can be
        let intersectionFolder map (ingredients, allergens) =
            List.fold (intersectionItemFolder ingredients) map allergens

        // recursive search allergens with single list ingredient
        let rec clarifyMap clarified rest =
            let clarifyFolder (acc, rest) (key, valueList) =
                let valueRest = List.fold (fun list (_, filter) -> List.except filter list) valueList acc
                if List.length valueRest = 1 then List.append acc [(key, valueRest)], rest
                    else acc, List.append rest [(key, valueRest)]

            let (acc, rest) = List.fold clarifyFolder (clarified, []) rest
            if acc = clarified then clarified
                else clarifyMap acc rest

        foods
        |> List.fold intersectionFolder Map.empty
        |> Map.toList
        |> List.map (fun (k, s) -> k, Set.toList s)
        |> clarifyMap []
        |> List.map (fun (k, h) -> k, List.head h)


    let parseInput input =
        input
        |> String.split "\n"
        |> Array.toList
        |> List.map parseFood

    module CaseA =
        let mayContains (foods:list<Food>):list<Ingredient*int> =
            let allIngredientsWithCount =
                foods |> List.map fst |> List.collect id |> List.groupBy id |> List.map (fun (el, grp) -> el, (List.length grp))

            let allergenIngredients =
                foods
                |> allergenMap
                |> List.map snd

            allIngredientsWithCount
            |> List.filter (fun (el,_) -> List.contains el allergenIngredients |> not)

    module CaseB =
        let canonicalString (foods:list<Food>):string =
            foods
            |> allergenMap
            |> List.sortBy fst
            |> List.map snd
            |> List.reduce (fun red cur -> $"{red},{cur}")
            |> String.trimChar ','

    [<Puzzle(2020, 21)>]
    let puzzle case (input:string) =
        input
        |> parseInput
        |>
        match case with
        | Case.A -> CaseA.mayContains >> List.sumBy snd >> string
        | Case.B -> CaseB.canonicalString
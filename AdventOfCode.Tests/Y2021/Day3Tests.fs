module AdventOfCode.Tests.Y2021.Day3Tests

open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Y2021.Day3
open Xunit
open Swensen.Unquote

[<Fact>]
let ``Split single item``() =
    let src = [ [ 1; 0; 1 ]]
    let (actualOxy, actualCO) = split src
    test <@ actualOxy = [[ 1; 0; 1 ]] @>
    test <@ actualCO = [] @>

[<Fact>]
let ``Split 2 items``() =
    let src = [
        [ 1; 0; 1 ]
        [ 0; 0; 1 ]
    ]

    let (actualOxy, actualCO) = split src
    test <@ actualOxy = [[ 1; 0; 1 ]] @>
    test <@ actualCO = [[ 0; 0; 1 ]] @>

[<Fact>]
let ``Split 3 item``() =
    let src = [
        [ 1; 0; 1 ]
        [ 0; 0; 1 ]
        [ 1; 0; 0 ]
    ]

    let (actualOxy, actualCO) = split src
    test <@ actualOxy = [[ 1; 0; 1 ]; [ 1; 0; 0 ]] @>
    test <@ actualCO = [[ 0; 0; 1 ]] @>

[<Fact>]
let ``Case B 3 item``() =
    let src = [
        [ 1; 0; 1 ]
        [ 0; 0; 1 ]
        [ 1; 0; 0 ]
    ]

    let (actualOxy, actualCO) = caseB src
    test <@ actualOxy = 5 @>
    test <@ actualCO = 1 @>

[<Fact>]
let ``Rate B 3 item``() =
    let src = [
        [ 1; 0; 1 ]
        [ 0; 0; 1 ]
        [ 1; 0; 0 ]
    ]

    let actual = rate fst 0L src
    test <@ actual = 5 @>
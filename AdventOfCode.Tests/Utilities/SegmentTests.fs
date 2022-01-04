module AdventOfCode.Tests.Utilities.SegmentTests

open AdventOfCode.Cases.Utilities
open AutoFixture.Xunit2
open Xunit
open Swensen.Unquote

[<Theory>]
[<AutoData>]
let ``No intersection`` x =
    let s1 = (x, x + 1)
    let s2 = (x + 2,x + 3)
    let actual = Segment.intersect s1 s2
    test <@ actual = ([s1], [], [s2]) @>

[<Theory>]
[<AutoData>]
let ``Single point intersection`` x =
    let s1 = (x, x + 1)
    let s2 = (x + 1, x + 2)
    let actual = Segment.intersect s1 s2
    test <@ actual = ([x,x], [x + 1, x + 1], [x + 2, x + 2]) @>

[<Theory>]
[<AutoData>]
let ``Intersect`` x =
    let s1 = (x - 1, x + 2)
    let s2 = (x + 1, x + 4)
    let actual = Segment.intersect s1 s2
    test <@ actual = ([x-1,x], [x + 1, x + 2], [x + 3, x + 4]) @>
    let actual = Segment.intersect s2 s1
    test <@ actual = ([x + 3, x + 4], [x + 1, x + 2], [x-1,x]) @>

[<Theory>]
[<AutoData>]
let ``Full overlap first`` x =
    let s1 = (x, x + 3)
    let s2 = (x + 1, x + 2)
    let actual = Segment.intersect s1 s2
    test <@ actual = ([x,x; x + 3, x + 3], [s2], []) @>
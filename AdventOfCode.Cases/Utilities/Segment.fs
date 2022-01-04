module AdventOfCode.Cases.Utilities.Segment

type Segment = int*int
type Segments = list<Segment>
type Segment2D = Segment*Segment
type Segment3D = Segment2D*Segment
type intersect<'a> = 'a->'a->list<'a>*list<'a>*list<'a>

let intersect s1 s2 =
    let isValid (v1,v2) = v1 <= v2
    match s1, s2 with
    | (mn1,mx1), (mn2,mx2) when mx2 >= mn1 && mx1 >= mn2 ->
        let fst = [(mn1, max mn1 mn2 - 1);(min mx1 mx2 + 1, mx1)] |> List.filter isValid
        let snd = [(mn2, max mn1 mn2 - 1);(min mx1 mx2 + 1, mx2)] |> List.filter isValid
        let crs = [max mn1 mn2,(min mx1 mx2)]
        fst,crs,snd
    | _ -> [s1],[],[s2]

let intersect2D s1 s2 =
    let x1,y1 = s1
    let x2,y2 = s2
    let fstx, crsx, sndx = intersect x1 x2
    let fsty, crsy, sndy = intersect y1 y2
    let fst = List.allPairs fstx fsty
    let snd = List.allPairs sndx sndy
    let crs = List.allPairs crsx crsy
    fst,crs,snd

let size2D s1 =
    let (xmn, xmx), (ymn,ymx) = s1
    (xmx - xmn + 1)*(ymx - ymn + 1)



module AdventOfCode.Cases.Y2023.Day24
open System
open System.Diagnostics
open System.Globalization
open System.Numerics
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open System.Threading.Tasks
open AdventOfCode.Cases.Infrastructure
open AdventOfCode.Cases.Infrastructure.Parser
open Microsoft.FSharp.Core

let toArray src =
    let size1 = Seq.length src
    let size2 = Seq.head src |> Seq.length
    let map = Array2D.create size1 size2 0.0
    [for i, row in Seq.indexed src do
         [for j, v in Seq.indexed row do map.[i,j]<-v]] |> ignore
    map  

let parse3 = String.split ", " >> function
    | [| Int64 x; Int64 y; Int64 z |] -> (float x),(float y), (float z)
    | _ -> failwith "wtf"

let parseLine = String.split " @ " >> function
    | [| positions; velocities |] -> (parse3 positions),(parse3 velocities)    
    | _ -> failwith "wtf"

type Cross = | No | Point of (float*float*float) | Parallel | Same

let intersect2 p1 p2 =
    let (x1, y1, z1), (vx1, vy1, vz1) = p1
    let (x2, y2, z2), (vx2, vy2, vz2) = p2
    let f1 = x1*vy1 - y1*vx1
    let f2 = x2*vy2 - y2*vx2
    let f = f2 - f1*vx2/vx1
    let k = (-vy2  + vx2 * vy1/vx1)
    if k = 0.0 && f = 0.0 then Same
    else if k = 0.0 then Parallel else
        let x = -f / k
        let y = (-f1 + vy1*x)/vx1
        let isFuture line (p,_) =
             let (x, _, _), (v, _, _) = line
             (x < p && v > 0.0) || (x > p && v < 0.0) || x = p
    
        if isFuture p1 (x,y) && isFuture p2 (x,y) then Point (x,y,0.0) else No
    
let isInside mn mx = function
    | Same -> true
    | Point (x,y,_) -> mn <= x && x <= mx && mn <= y && y <= mx
    | _ -> false    
   
let skipCol (source:float[,]) (target:float[,]) col i j _ =
    if i = 0 then () else if j < col then target.[i - 1, j]<-source.[i, j] else if j > col then target.[i - 1, j - 1]<-source.[i, j] else ()
    
let rec det matrix =
    let size1, size2 = Array2D.sizes matrix
    if size1 <> size2 then failwith "not symmetric"  else ()
    if size1 = 0 then matrix.[0, 0] else
        [for ind in 0..size1 do
            let nested = Array2D.init size1 size1 (fun i j -> 0.0)
            Array2D.iteri (skipCol matrix nested ind) matrix
            let sign = if ind % 2 = 0 then 1.0 else -1.0
            let dt = det nested
            let a = matrix.[0, ind]
            yield sign*a*dt
        ] |> List.sum
        
let coeffXY (p1,p2) =                       
    let (x1, y1, z1), (vx1, vy1, vz1) = p1   
    let (x2, y2, z2), (vx2, vy2, vz2) = p2
    [
        y1 - y2 //Vx
        x2 - x1 // Vy
        vy2 - vy1 // x
        vx1 - vx2 // y        
    ], (vx1 * y1 - vx2*y2 - x1 * vy1 + x2*vy2)
    
let coeffXZ (p1,p2) =                            
    let (x1, y1, z1), (vx1, vy1, vz1) = p1       
    let (x2, y2, z2), (vx2, vy2, vz2) = p2       
    [                                            
        z1 - z2 //Vx                             
        x2 - x1 // Vz                            
        vz2 - vz1 // x                           
        vx1 - vx2 // z                           
    ], (vx1 * z1 - vx2*z2 - x1 * vz1 + x2*vz2)   
  
let swap matrix col substitute =
    let matrix = Array2D.copy matrix
    Array.iteri (fun i v -> matrix.[i, col]<-v) substitute
    matrix
    
let rec take4Rnd choose src =
    if List.length choose = 4 then choose else
        let p1,p2 = Seq.head src
        let choose =
            if List.exists (fun (v1,v2) -> v1= p1 || v1 = p2 ) choose then  choose else
                (p1,p2)::choose
        take4Rnd     choose  <| Seq.tail src
    
    
[<Puzzle(2023, 24)>]
let puzzle case (source:seq<string>) =
    let source = source |> Seq.map parseLine |> Seq.toList
    let allPairs = List.allPairs source source |> List.filter (fun (a,b) -> a > b)
        
    match case with
    | Case.A ->
        allPairs
        |> List.map (fun (a,b) -> intersect2 a b)
        |> List.filter (isInside 200000000000000.0 400000000000000.0) 
        |> List.length   |> float
    | Case.B ->
                   
        let matrixData = allPairs |> take4Rnd []   |> List.map coeffXY 
        let matrix = List.map fst matrixData    |> toArray 
        let freeCoeff = List.map snd matrixData    |> Seq.toArray
        let detM = det matrix
        let x = swap matrix 2 freeCoeff |> det |> (/) <| detM
        let y = swap matrix 3 freeCoeff |> det |> (/) <| detM
        let matrixData = allPairs |> take4Rnd []   |> List.map coeffXY
        
        let matrixData = allPairs |> take4Rnd []   |> List.map coeffXZ
        let matrix = List.map fst matrixData    |> toArray              
        let freeCoeff = List.map snd matrixData    |> Seq.toArray       
        let detM = det matrix                                            
        let z = swap matrix 3 freeCoeff |> det |> (/) <| detM
        x + y + z
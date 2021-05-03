module Lectures.Lecture4

open System

// http://www.fssnip.net/o7/title/Simple-normally-distributed-random-number-generator
let normalDistRandom mean stdDev seed = 
    let rand = Random(seed)
    let rec polarBoxMullerDist () = seq {
            let rec getRands () =
                let u = (2.0 * rand.NextDouble()) - 1.0
                let v = (2.0 * rand.NextDouble()) - 1.0
                let w = u * u + v * v
                if w >= 1.0 then
                    getRands()
                else
                    u, v, w
            let u, v, w = getRands()
            
            let scale = Math.Sqrt(-2.0 * Math.Log(w) / w)
            let x = scale * u
            let y = scale * v
            yield mean + (x * stdDev); yield mean + (y * stdDev); yield! polarBoxMullerDist ()
        }
    polarBoxMullerDist ()
    

let getGeometricMotionSeq n S0 r vol t seed =
    let div = ((float t) / (float n))
    let x = sqrt(div)
    let myNormalDistRandom = normalDistRandom 0. div seed
    let randomVariables = Seq.take n myNormalDistRandom
    let SSeq = (S0, randomVariables) |> Seq.unfold(fun (a, b) ->
            if (Seq.isEmpty b) then None
            else
                let exponent = (r - (pown vol 2) / 2.) * div + vol * x * (Seq.head b)
                let next = a * Math.Pow(Math.E, exponent)
                Some(a, (next, Seq.tail b))
        )
    SSeq

let getHistoricalVolatility (geometricMotionSeq: seq<float>) t n =
    let RSeq = geometricMotionSeq |> Seq.pairwise |> Seq.map (fun (a, b) -> log(b / a))
    let RAvg = Seq.average RSeq
    let volatility = sqrt (( n / (t * (n-1.)) ) * (RSeq |> Seq.map (fun r -> pown (r - RAvg) 2) |> Seq.sum))
    volatility
    
let zad1 N n S0 r vol t seed =
    let paths = List.init N (fun i -> getGeometricMotionSeq n S0 r vol t (seed + i))
    let resultStrings = paths |> List.map (fun GM ->
        (Seq.last GM |> string) + "," + (getHistoricalVolatility GM t (float n) |> string)
    )
    let wr = new System.IO.StreamWriter("""E:\data\output.txt""")
    resultStrings |> String.concat("\n") |> wr.Write
    wr.Close()

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
    
//let zad1 N n S0 r vol t seed =
//    let paths = List.init N (fun i -> getGeometricMotionSeq n S0 r vol t (seed + i))
//    for path in paths
//        path |> printfn "%A"
//    0
        
//    SSeq |> Seq.length |> printfn "%A"
//    SSeq |> Seq.take n |> printfn "%A"
//    SSeq |> Seq.take n |> Seq.length |> printfn "%A"
    
//    let wr = new System.IO.StreamWriter("""E:\data\br_motion.csv""")
//    SSeq |> Seq.take n |> Seq.map(string) |> String.concat(",") |> wr.Write
//    wr.Close()
    
//    let t = 0
    
//    let Wt = W t 0. myNormalDistRandom
//    let exponent = (drift -. (pown vol 2)) * t + vol * Wt
//    let result = price * Math.Pow(Math.E, exponent)
    //    result
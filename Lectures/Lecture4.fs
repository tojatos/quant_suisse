module Lectures.Lecture4

open System

let normalDistRandom mean stdDev = 
    let rand = Random()
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

//let zad1 count steps price drift vol years seed =
//    let r = Random(seed)
//    let exponent = (drift - pown vol 2) * 0
//    let result = price * Math.Pow(Math.E, )
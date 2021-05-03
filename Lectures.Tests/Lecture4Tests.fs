module Lectures.Tests.Lecture4Tests

open NUnit.Framework
open Lectures.Lecture4
open FsUnit

[<Test>]
let ``normalDistRandom distribution test`` () =
    let x = normalDistRandom 0. 0.5 1000
    let wr = new System.IO.StreamWriter("E:\data\dist_test.csv")
    Seq.take 1000 x |> Seq.map(string) |> String.concat(",") |> wr.Write
    wr.Close()
    
let replaceString (oldValue:string) (newValue:string) (message:string) =
    message.Replace(oldValue, newValue)
    
[<Test>]
let ``geometric brownian motion test`` () =
    //let            getGeometricMotionSeq  n   S0   r    vol t seed
    let resultStrings = List.init 10 (fun i ->
        let result = getGeometricMotionSeq 300 300. 0.07 0.3 1. (1 + i)
        result |> Seq.take 300 |> Seq.map(string) |> String.concat("\t") |> replaceString "." ","
        )
    let wr = new System.IO.StreamWriter("""E:\data\br_motion.csv""")
    resultStrings |> String.concat("\n") |> wr.Write
    wr.Close()
    
[<Test>]
let ``zad1 test`` () =
    //   N  n   S0    r   vol t seed
    zad1 10 300 300. 0.07 0.3 1. 1

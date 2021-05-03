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
    true |> should equal true
    
let replaceString (oldValue:string) (newValue:string) (message:string) =
    message.Replace(oldValue, newValue)
    
[<Test>]
let ``geometric brownian motion test`` () =
    //let            getGeometricMotionSeq  n   S0   r    vol t seed
    let resultStrings = List.init 10 (fun i ->
        let result = getGeometricMotionSeq 300 300. 0.07 0.3 1 (1 + i)
        result |> Seq.take 300 |> Seq.map(string) |> String.concat("\t") |> replaceString "." ","
        )
//    let result = getGeometricMotionSeq 300 300. 0.01 0.2 1 1
    let wr = new System.IO.StreamWriter("""E:\data\br_motion.csv""")
    resultStrings |> String.concat("\n") |> wr.Write
//    result |> Seq.take 300 |> Seq.map(string) |> String.concat(" ") |> replaceString "." "," |> wr.Write
    wr.Close()
    true |> should equal true
    
//[<Test>]
//let ``zad1 test`` () =
//    //let        zad1 N n   S0    r    vol t seed
//    let result = zad1 1 300 300. 0.01 0.2 1 1
//    true |> should equal true

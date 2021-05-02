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
    
[<Test>]
let ``zad1 test`` () =
    let result = zad1 1 100 0 0 0 5 1000
    true |> should equal true

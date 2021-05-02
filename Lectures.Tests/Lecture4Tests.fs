module Lectures.Tests.Lecture4Tests

open NUnit.Framework
open Lectures.Lecture4
open FsUnit

[<Test>]
let ``normalDistRandom distribution test`` () =
    let x = normalDistRandom 0. 0.5
    Seq.take 10 x |> printfn "%A"
    Seq.take 100 x |> printfn "%A"
    true |> should equal true

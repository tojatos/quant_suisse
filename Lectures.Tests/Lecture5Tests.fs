module Lectures.Tests.Lecture5Tests

open NUnit.Framework
open Lectures.Lecture5
open FsUnit

[<Test>]
let ``black scholes test`` () =
    let x = black_scholes Call 60.0 65.0 0.25 0.08 0.3
    printfn "%A" x


// s: stock price
// x: strike price of option
// t: time to expiration in years
// r: risk free interest rate
// v: volatility
[<Test>]
let ``zad2`` () =
    let stockPrices = seq {for i in 1. .. 1000. -> i}
    let expirations = [0.001; 0.5; 1.]
    let strike = 140.
    let volatility = 0.1
    let r = 0.3
    let types = [Call; Put]
    let wr = new System.IO.StreamWriter("""E:\data\call.csv""")
    for expiration in expirations do
        expiration |> wr.WriteLine
        for t in types do
            t |> wr.WriteLine
            let data = stockPrices |> Seq.map (fun x -> black_scholes t x strike expiration r volatility)
            data |> Seq.map(fst) |> Seq.map(string) |> String.concat(",") |> wr.Write
            "" |> wr.WriteLine
            data |> Seq.map(snd) |> Seq.map(string) |> String.concat(",") |> wr.Write
            "" |> wr.WriteLine
    wr.Close()
    
[<Test>]
let ``zad3`` () =
    let stockPrices = seq {for i in 1. .. 1000. -> i}
    let expirations = [0.001; 0.5; 1.]
    let strike = 140.
    let volatility = 0.2
    let r = 0.3
    let types = [Call; Put]
    let wr = new System.IO.StreamWriter("""E:\data\call3.csv""")
    for expiration in expirations do
        expiration |> wr.WriteLine
        for t in types do
            t |> wr.WriteLine
            let data = stockPrices |> Seq.map (fun x -> black_scholes t x strike expiration r volatility)
            data |> Seq.map(fst) |> Seq.map(string) |> String.concat(",") |> wr.Write
            "" |> wr.WriteLine
            data |> Seq.map(snd) |> Seq.map(string) |> String.concat(",") |> wr.Write
            "" |> wr.WriteLine
    wr.Close()
    

module Lectures.Tests.Lecture2Tests

open NUnit.Framework
open Lectures.Lecture2
open FsUnit

[<Test>]
let ``Hands are judged correctly``() =
    judgeHand [ Card (Ace, Heart);     Card (Value 7, Spade); Card (Value 3, Club) ] |> should equal Blackjack
    judgeHand [ Card (Ace, Heart);     Card (Value 7, Spade); Card (King, Club) ]    |> should equal Over21
    judgeHand [ Card (Value 1, Heart); Card (Value 7, Spade); Card (King, Club) ]    |> should equal (Hand 18)

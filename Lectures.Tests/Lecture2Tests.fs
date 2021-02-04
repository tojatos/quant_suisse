module Lectures.Tests.Lecture2Tests

open NUnit.Framework
open Lectures.Lecture2
open FsUnit

[<Test>]
let ``Hands are judged correctly`` () =
    judgeHand [ Card(Ace, Heart)
                Card(Value 7, Spade)
                Card(Value 3, Club) ]
    |> should equal Blackjack

    judgeHand [ Card(Ace, Heart)
                Card(Value 7, Spade)
                Card(King, Club) ]
    |> should equal Over21

    judgeHand [ Card(Value 1, Heart)
                Card(Value 7, Spade)
                Card(King, Club) ]
    |> should equal (Hand 18)

[<Test>]
let ``Folds are working properly`` () =
    let list1 = [ 1; 2; 3; 4; 5 ]
    let list2 = [ -1; -20; 3; -1; 4 ]
    let test1 f = f (+) 10 []
    let test2 f = f (+) 0 list1
    let test3 f = f (+) 10 list2
    let test4 f = f (-) 20 list1
    let test5 f = f (-) 20 list2

    let testB1 f = f (+) [] 10
    let testB2 f = f (+) list1 0
    let testB3 f = f (+) list2 10
    let testB4 f = f (-) list1 20
    let testB5 f = f (-) list2 20

    let testFold case =
        case listFold |> should equal (case List.fold)

    let testFoldBack case =
        case listFoldBack
        |> should equal (case List.foldBack)

    testFold test1
    testFold test2
    testFold test3
    testFold test4
    testFold test5
    testFoldBack testB1
    testFoldBack testB2
    testFoldBack testB3
    testFoldBack testB4
    testFoldBack testB5

[<Test>]
let ``Primitive parsers work`` () =
    Parsers.pChar 'd' "dolphin" |> Option.get |> should equal ('d', "olphin")
    Parsers.pChar 'e' "dolphin" |> should equal None

    Parsers.pAny "dolphin" |> Option.get |> should equal ('d', "olphin")
    Parsers.pAny "" |> should equal None

    Parsers.pDigit "4dolphins" |> Option.get |> should equal (4, "dolphins")
    Parsers.pDigit "30dolphins" |> Option.get |> should equal (3, "0dolphins")
    Parsers.pDigit "dolphins4" |> should equal None
    Parsers.pDigit "" |> should equal None

    Parsers.pSpace " " |> Option.get |> should equal (" ", "")
    Parsers.pSpace "   test" |> Option.get |> should equal ("   ", "test")
    Parsers.pSpace "   test   " |> Option.get |> should equal ("   ", "test   ")
    Parsers.pSpace "" |> should equal None
    Parsers.pSpace "ala ma kota" |> should equal None

    Parsers.pWord "d" "dolphin" |> Option.get |> should equal ("d", "olphin")
    Parsers.pWord "ala" "ala ma kota" |> Option.get |> should equal ("ala", " ma kota")
    Parsers.pWord "kota" "ala ma kota" |> should equal None
    Parsers.pWord "c" "" |> should equal None
    

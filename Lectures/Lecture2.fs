module Lectures.Lecture2

type Suit =
    | Heart
    | Diamond
    | Club
    | Spade

type Rank =
    | Ace
    | King
    | Queen
    | Jack
    | Value of int

type Card = Card of Rank * Suit

type HandType =
    | Over21
    | Hand of int
    | Blackjack

let getValue (card: Card) =
    match card with
    | Card (Ace, _) -> 11
    | Card (King, _)
    | Card (Queen, _)
    | Card (Jack, _) -> 10
    | Card (Value x, _) -> x

let getHandType (value: int) =
    match value with
    | 21 -> Blackjack
    | value when value > 21 -> Over21
    | _ -> Hand(value)

let judgeHand (cards: list<Card>) = List.sumBy getValue cards |> getHandType

let rec listFold func state list =
    match list with
    | head :: tail -> listFold func (func state head) tail
    | [] -> state

let listFoldBack func list state =
    let rec recListFoldBack f l s =
        match l with
        | head :: tail -> recListFoldBack f tail (f head s)
        | [] -> s
    recListFoldBack func (List.rev list) state
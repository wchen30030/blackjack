{- 
  File      :  Deck.hs 
  Copyright : (c) Wei Chen, 02/11/17 
  Contains Card, Deck, Hand, oneDeck, cardValue
-}

module Deck
(
    Card(..),
    Deck(..),
    Hand(..),
    oneDeck,
    cardValue

) where


data Card = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine |
            Ten | Jack | Queen | King 
            deriving (Show, Eq, Enum)
             
type Deck = [Card]
type Hand = [Card]

{-complete deck of 52 cards-}
oneDeck :: Deck
oneDeck = [Ace .. King]++[Ace .. King]++[Ace .. King]++[Ace .. King]


{-value of each card. We set ace to 1 but keep track elsewhere-}
cardValue :: Card -> Int
cardValue Ace = 1
cardValue Two = 2
cardValue Three = 3
cardValue Four = 4
cardValue Five = 5
cardValue Six = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine = 9
cardValue _ = 10


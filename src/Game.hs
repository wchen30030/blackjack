{- 
  File      :  Game.hs 
  Copyright : (c) Wei Chen, 02/11/17 
  Contains Player, GameStatus, Game, initGame, deal, softHand, bjHand,
  handValue
-}


module Game
(
    Player(..),
    GameStatus(..),
    Game(..),
    initGame,
    deal,
    softHand,
    bjHand,
    handValue
) where

import Data.List
import Deck

{- No Need to separate the dealer and player into their own modules or different types.
Changed Int to Hand.  -}
data Player = Player Hand 
    deriving (Show)

{- Depending on how you implement your program you may not need this. This is here to provide the BlackJack.hs 
   information about the game state. But you could just place this inside the Game data-type -}
data GameStatus = PlayerTurn | DealerTurn | GameOver
    deriving (Show, Eq, Enum)

{- All the information you'll need for the game. This can be passed around between BlackJack.hs and Game.hs. 
   The first "player" is the user and the second "Player" is the dealer -}
data Game = Game Player Player GameStatus Deck
    deriving (Show)

{- initializes a game. player first, dealer second-}
initGame :: Deck -> Game
initGame deck = Game (Player []) (Player []) PlayerTurn deck 


{-deal num number of cards to a hand and remove from deck-}
deal :: Int -> Deck -> (Hand, Deck)
deal num deck = (take num deck, drop num deck)


{-check if a hand has an ace-}
softHand :: Hand -> Bool
softHand hand = (elem Ace hand)


{-check if hand is blackjack. Only done once initially with 2 cards-}
bjHand :: Hand -> Bool
bjHand [card1, card2] = 
            (card1==Ace && elem card2 [Ten, Jack, Queen, King]) ||
            (card2==Ace && elem card1 [Ten, Jack, Queen, King])


{-highest non-bust value of hand.
value of 0 on bust-}
handValue :: Hand -> Int
handValue hand =
             if softHand hand
                  then if 10+value <=21
                    then 10+value
                  else
                    if value <=21
                        then value
                    else
                        0
             else
                  if value > 21
                    then 0
                  else
                    value
            where value = sum(map cardValue hand)



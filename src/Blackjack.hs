{- 
  File      :  Blackjack.hs 
  Copyright : (c) Wei Chen, 02/11/17 
  imports from Shuffle, Deck, and Game and runs blackjack
  contains hitOrStay, showCards, showPlayer, showDealer,
  gameLoop, replay, main, initHands, checkBJ, playerMove,
  dealerMove, decideWinner
-}

module Main where

import Data.List
import System.Environment 
import System.IO
import Control.Monad
import Shuffle
import Deck
import Game


{- You can create little IO actions like this for the main menu -}
hitOrStay :: IO Int
hitOrStay = do 
    putStrLn "What would you like to do?"
    putStrLn "(1) Hit"
    putStrLn "(2) Stay"
    {- Remember getLine returns a string -}
    optStr <- getLine 
    {- This converts the string into Integer -}
    let optionInt = read optStr :: Int 
    return optionInt 


{-show player's and dealer's hands-}
showCards :: Game -> IO()
showCards (Game (Player p) (Player d) status deck) = do
    putStr "Player's hand: " 
    showPlayer p
    putStr "Dealer's hand: "
    showDealer d status


{-show player's hand-}
showPlayer :: Hand -> IO ()
showPlayer hand = do
    putStrLn (show hand)

{-show dealer's hand depending on status 
to determine if reveal first card-}
showDealer :: Hand -> GameStatus -> IO ()
showDealer (x:xs) status = do
    if status == PlayerTurn
        then do
            putStrLn (show xs)
        else
            putStrLn (show (x:xs))


{-deals out first hand to player and dealer-}
initHands :: Game -> IO Game
initHands (Game (Player p) (Player d) status deck) = do
    let (p',deck') = (deal 2 deck)
    let (d',deck'') = (deal 2 deck')
    return (Game (Player p') (Player d') status deck'')


{-check if there is a  blackjack winner-}
checkBJ :: Game -> IO Bool
checkBJ (Game (Player p) (Player d) status deck) = do
    if bjHand p && bjHand d
        then do putStrLn "Tie Game"
                putStrLn ""
                return True
    else if bjHand p
        then do putStrLn "Player Wins"
                putStrLn ""
                return True
    else if bjHand d
        then do putStrLn "Dealer Wins"
                putStrLn ""
                return True
    else
        return False


{-player plays until bust or stay-}
playerMove :: Game -> IO Game
playerMove (Game (Player p) (Player d) status deck) = do
    optP <- hitOrStay
    if optP /= 1
        then do return (Game (Player p) (Player d) DealerTurn deck)
    else do
        let (temp1,temp2) = (deal 1 deck)
        let (p',deck') = (p++temp1,temp2)
        showCards (Game (Player p') (Player d) status deck')
        if handValue p' == 0
           then do putStrLn "Dealer Wins"
                   putStrLn ""
                   return (Game (Player p') (Player d) GameOver deck')
        else do playerMove (Game (Player p') (Player d) status deck')


{-dealer plays until bust or soft 17-}
dealerMove :: Game -> IO Game
dealerMove (Game (Player p) (Player d) status deck) = do
    showCards (Game (Player p) (Player d) status deck)
    if handValue d == 0
        then do 
            putStrLn "Player Wins"
            putStrLn ""
            return (Game (Player p) (Player d) GameOver deck)
    else if (handValue d >=18 || (handValue d ==17 && softHand d == False))
        then do
            return (Game (Player p) (Player d) status deck)
    else do
            let (temp1,temp2) = (deal 1 deck)
            let (d',deck') = (d++temp1,temp2)
            dealerMove (Game (Player p) (Player d') status deck')


{-decide winner at end-}
decideWinner :: Game -> IO ()
decideWinner (Game (Player p) (Player d) status deck) = do
    if status /= GameOver
        then if handValue p == handValue d
            then do
                putStrLn "Tie Game"
                putStrLn ""
        else if handValue p > handValue d
            then do
                putStrLn "Player Wins"
                putStrLn ""
        else do putStrLn "Dealer Wins"
                putStrLn ""
    else do
        return ()


{-loops through all operations in one game-}
gameLoop :: Game -> IO ()
gameLoop game1 = do 
    (Game (Player p2) (Player d2) status2 deck2) <- initHands game1
    showCards (Game (Player p2) (Player d2) status2 deck2)
    check1 <- checkBJ (Game (Player p2) (Player d2) status2 deck2)
    if check1
        then do showCards (Game (Player p2) (Player d2) GameOver deck2)
                return ()
    else do (Game (Player p3) (Player d3) status3 deck3) <- playerMove (Game (Player p2) (Player d2) status2 deck2)
            if status3 == GameOver
                then do return ()
            else do game4 <- dealerMove (Game (Player p3) (Player d3) status3 deck3)
                    decideWinner game4
                    return ()


{-gives player option of playing or quitting-}
replay :: IO ()
replay = do
    putStrLn "NEW GAME"
    putStrLn "What would you like to do?"
    putStrLn "(1) Play"
    putStrLn "(2) Quit"
    playStr <- getLine 
    let playInt = read playStr :: Int 
    if playInt == 1
        then do 
            initDeck <- shuffle oneDeck
            let game = initGame initDeck  
            gameLoop game
            replay
        else do 
            return ()


{-initializes the game-}
main :: IO () 
main = do 
    putStrLn "Welcome to the game of BlackJack!"
    replay


module Main where

import Control.Monad
import Control.Monad.State
import Data.Array
import Tiles
import Utils

-- The state of the game at any given point in time
data GameState = GS
  { _deck :: Deck,
    _hands :: Array Int Hand
  }
  deriving (Show)

-- Draws a hand from the deck. State transformation where the deck is the state
takeHand :: State Deck Hand
takeHand = do
  deck <- get
  let hand = take 13 deck
  put $ drop 13 deck
  return $ listArray (0, 12) hand

-- Creates a new game state (i.e., shuffles the deck, deals hands to players etc)
newGameState :: IO GameState
newGameState = do
  shuffledDeck <- shuffle baseDeck
  let (hands, deck) = runState (listArray (0, 3) <$> replicateM 4 takeHand) shuffledDeck
  return GS {_deck = deck, _hands = hands}

main :: IO ()
main = do
  putStrLn "Welcome to MAHJONG"
  state <- newGameState
  print state

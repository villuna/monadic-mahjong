{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Functor
import Data.List
import System.IO
import Text.Parsec hiding (State)
import Tiles
import Utils

-- The state of the game at any given point in time
data GameState = GameState
  { _deck :: Deck,
    _hands :: [Hand]
  }
  deriving (Show)

makeLenses ''GameState

-- Draws a hand from the deck. State transformation where the deck is the state
takeHand :: State Deck Hand
takeHand = do
  deck <- get
  let hand = sort $ take 13 deck
  put $ drop 13 deck
  return hand

-- Creates a new game state (i.e., shuffles the deck, deals hands to players etc)
newGameState :: IO GameState
newGameState = do
  shuffledDeck <- shuffle baseDeck
  let (hands, deck) = runState (replicateM 4 takeHand) shuffledDeck
  return GameState {_deck = deck, _hands = hands}

-- Types of moves the player can make on their turn
-- Either the player can keep the new tile they picked up (discarding the old tile) or reject the
-- tile and not pick it up.
--
-- I will add kan and riichi later on
data Move = Keep Int | Reject deriving (Show)

parsePlayerMove :: String -> Maybe Move
parsePlayerMove move = case parse playerMove "" move of
  Right m -> Just m
  Left _ -> Nothing
  where
    playerMove :: Parsec String () Move
    playerMove =
      (char 'R' $> Reject)
        <|> ( string "K " *> do
                i <- read <$> many1 digit
                if i `elem` [1 .. 13]
                  then
                    return $ Keep (i - 1)
                  else
                    fail "index out of range"
            )

getPlayerMove :: IO Move
getPlayerMove = do
  putStr "Input your move ([R]eject, [K]eep <index of tile to discard, 1-13>): "
  hFlush stdout
  move <- parsePlayerMove <$> getLine
  maybe (putStrLn "Invalid move, try that again." >> getPlayerMove) return move

executeKeep :: (Monad m) => Int -> Int -> Tile -> StateT GameState m ()
executeKeep player i drawn = do
  hand <- use (hands . element player)
  let newHand = sort $ hand & element i .~ drawn
  (hands . element player) .= newHand

executePlayerMove :: (Monad m) => Int -> Move -> StateT GameState m ()
executePlayerMove player move = do
  drawn <- head <$> use deck

  case move of
    Keep i -> executeKeep player i drawn
    _ -> return ()

  deck %= tail

playerTurn :: StateT GameState IO ()
playerTurn = do
  state <- get
  let handStr = showHand $ head (state ^. hands)
  lift . putStrLn $ "Your hand: " ++ handStr ++ " " ++ show (head (state ^. deck))
  move <- lift getPlayerMove
  executePlayerMove 0 move

gameLoop :: StateT GameState IO ()
gameLoop = do
  playerTurn
  gameLoop

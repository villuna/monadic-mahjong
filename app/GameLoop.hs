module GameLoop where

import Control.Lens
import Control.Monad.State
import Data.List
import Game
import Tiles

executeKeep :: (Monad m) => Int -> Int -> Tile -> StateT GameState m ()
executeKeep player i drawn = do
  playerHand <- use (players . element player . hand)
  let newHand = sort $ playerHand & element i .~ drawn
  (players . element player . hand) .= newHand

executePlayerMove :: Int -> Move -> StateT GameState IO ()
executePlayerMove player move = do
  drawn <- head <$> use deck

  case move of
    -- The print statements are just here so I can see the AIs are actually doing something
    Keep i -> lift (putStrLn ("player " ++ show player ++ " played keep " ++ show (i + 1))) >> executeKeep player i drawn
    _ -> lift . putStrLn $ "player " ++ show player ++ " discarded"

  deck %= tail

playerTurn :: StateT GameState IO ()
playerTurn = do
  state <- get
  let player = state ^. currentPlayer
  let controller = state ^?! playerControllers . element player
  move <- lift $ controller player state
  executePlayerMove player move
  currentPlayer %= flip mod 4 . (+ 1)

gameLoop :: StateT GameState IO ()
gameLoop = do
  playerTurn
  gameLoop

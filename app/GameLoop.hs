module GameLoop where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List
import Game
import Tiles

executeKeep :: Int -> Int -> Tile -> StateT GameState IO ()
executeKeep player i drawn = do
  playerHand <- use (players . element player . hand)
  let newHand = sort $ playerHand & element i .~ drawn
  (players . element player . hand) .= newHand

executeKan :: Int -> Tile -> Tile -> StateT GameState IO ()
executeKan player drawn tile = do
  -- When we kan we keep the new tile that we drew and then remove all 4 of the kan tile from our
  -- hand.
  let playerHand = players . element player . hand
  playerHand %= sort . (drawn :)
  replicateM_ 4 $ playerHand %= delete tile
  players . element player . calls %= (Call (replicate 4 tile) ClosedKan :)

-- Returns a bool indicating whether the player gets to move again (in case of Kan)
executePlayerMove :: Int -> Move -> StateT GameState IO Bool
executePlayerMove player move = do
  state <- get
  let drawn = head $ state ^. deck
  let playerState = state ^?! players . element player

  again <- case move of
    -- The print statements are just here so I can see the AIs are actually doing something
    Take i -> do
      let tile = playerState ^?! hand . element i
      lift . putStrLn $ "Player " ++ show player ++ " discarded tile " ++ show tile
      executeKeep player i drawn
      return False
    Reject -> do
      lift . putStrLn $
        "Player "
          ++ show player
          ++ " discarded tile "
          ++ show drawn
      return False
    MakeKan tile -> do
      if tile `elem` kanTiles playerState drawn
        then do
          lift . putStrLn $
            "Player "
              ++ show player
              ++ " made a closed Kan : "
              ++ concat (replicate 4 (show tile))
          executeKan player drawn tile
        else undefined -- should never happen (but i could probably handle this more gracefully)
      return True

  deck %= tail
  return again

playerTurn :: StateT GameState IO ()
playerTurn = do
  state <- get
  let player = state ^. currentPlayer
  let controller = state ^?! playerControllers . element player
  move <- lift $ controller player state
  again <- executePlayerMove player move

  if again
    then
      playerTurn
    else
      currentPlayer %= flip mod 4 . (+ 1)

gameLoop :: StateT GameState IO ()
gameLoop = do
  playerTurn
  gameLoop

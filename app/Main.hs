module Main where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List (sort)
import Game
import GameLoop
import Tiles

main :: IO ()
main = do
  putStrLn "Welcome to MAHJONG"
  state <- newGameState
  let newState = state & players . element 0 . hand %~ (\h -> sort $ replicate 4 (Dragon Red) ++ drop 4 h)
  void $ execStateT gameLoop newState

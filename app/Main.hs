module Main where

import Control.Monad
import Control.Monad.State
import Game
import GameLoop

main :: IO ()
main = do
  putStrLn "Welcome to MAHJONG"
  state <- newGameState
  void $ execStateT gameLoop state

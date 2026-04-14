module Enemy where

import Control.Monad.State
import Game
import System.Random
import Tiles

-- Operation that calculates which move the AI makes on their turn.
type CalculateMove i = Tile -> Int -> GameState -> StateT () IO Move

-- An extremely bad ai that does moves at random and can expect to lose every game.
randomMove :: CalculateMove ()
randomMove _ _ _ = do
  choice <- lift $ randomRIO (0, 13)

  case choice of
    13 -> return Reject
    i -> return $ Keep i

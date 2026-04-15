module GameLoop where

import Control.Lens
import Control.Monad.State
import Data.Functor
import Data.List
import Game
import System.IO
import Text.Parsec hiding (State)
import Tiles

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
  playerHand <- use (players . element player . hand)
  let newHand = sort $ playerHand & element i .~ drawn
  (players . element player . hand) .= newHand

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
  let handStr = showHand $ state ^. players . element 0 . hand
  lift . putStrLn $ "Your hand: " ++ handStr ++ " " ++ show (head (state ^. deck))
  lift . print $ possibleSegmentations (state ^?! players . element 0) (head (state ^. deck))
  move <- lift getPlayerMove
  executePlayerMove 0 move

gameLoop :: StateT GameState IO ()
gameLoop = do
  playerTurn
  gameLoop

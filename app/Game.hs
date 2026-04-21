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
import System.Random
import Text.Parsec hiding (State)
import Tiles
import Utils

-- A call is a set of tiles the player has put on the table separate from their hand. A chii is a
-- run of 3 number tiles in sequence. Pon is 3 of a kind, and Kan is 4 of a kind. Kan can be open
-- or closed, depending on whether the any of the tiles were stolen from another player.
data Call = Call [Tile] CallType deriving (Show)

callTiles :: Call -> [Tile]
callTiles (Call tiles _) = tiles

data CallType = Pon | Chii | OpenKan | ClosedKan deriving (Eq, Show)

-- Operation that calculates which move the agent (human player or AI) makes on their turn.
type AgentController = Int -> GameState -> IO Move

data PlayerState = PlayerState
  { _hand :: [Tile],
    _calls :: [Call],
    _seatWind :: Direction
  }
  deriving (Show)

-- An extremely bad ai that does moves at random and can expect to lose every game.
randomMove :: AgentController
randomMove _ _ = do
  choice <- randomRIO (0, 13)

  case choice of
    13 -> return Reject
    i -> return $ Keep i

-- The state of the game at any given point in time
data GameState = GameState
  { _deck :: Deck,
    _players :: [PlayerState],
    _playerControllers :: [AgentController],
    _prevailingWind :: Direction,
    _currentPlayer :: Int
  }

-- Types of moves the player can make on their turn
-- Either the player can keep the new tile they picked up (discarding the old tile) or reject the
-- tile and not pick it up.
--
-- I will add kan and riichi later on
data Move = Keep Int | Reject deriving (Show)

makeLenses ''GameState
makeLenses ''PlayerState

newPlayer :: [Tile] -> Direction -> PlayerState
newPlayer hand seatWind = PlayerState {_hand = hand, _calls = [], _seatWind = seatWind}

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
  seats <- shuffle [North, East, South, West]
  return
    GameState
      { _deck = deck,
        _players = zipWith newPlayer hands seats,
        _playerControllers = playerMove : replicate 3 randomMove,
        _prevailingWind = East,
        _currentPlayer = 0
      }

-- A Yaku is a condition on a hand which gives a certain number of han when satisfied
-- the number of points may depend on the hand
--
-- If the function returns Nothing, the yaku is not satisfied. Otherwise returns the number of han
-- the function satisfies.
type Yaku = GameState -> PlayerState -> Tile -> Maybe Int

-- Returns if the player is in tenpai - i.e., the player is one tile away from making a hand.
-- Returns a list of which tiles would complete the players hand.
tenpai :: PlayerState -> [Tile]
tenpai player = filter (isValidHand player) quarterDeck

-- Returns whether the player can make a hand with the tiles they have plus the newly drawn tile.
-- This does not mean the hand satisfied a yaku - merely that it has the shape of a valid hand.
--
-- TODO I bet I've missed some valid hands because mahjong is very complicated, but I will add them
-- if I find them in my research.
isValidHand :: PlayerState -> Tile -> Bool
isValidHand player tile = any hasValidStructure (possibleSegmentations player tile)
  where
    hasValidStructure :: SegmentedHand -> Bool
    hasValidStructure hand = regularStructure hand || allPairs hand

    hasStructure :: [Int] -> SegmentedHand -> Bool
    hasStructure structure = (== structure) . sort . map length

    regularStructure = hasStructure [2, 3, 3, 3, 3]
    allPairs = hasStructure [2, 2, 2, 2, 2, 2, 2]

-- A hand split up into sets
type SegmentedHand = [TileSet]

type TileSet = [Tile]

-- Returns all the possible ways the player's hand can be split up into tile sets.
possibleSegmentations :: PlayerState -> Tile -> [SegmentedHand]
possibleSegmentations player tile = map (callSets ++) (segmentationsH (tile : (player ^. hand)))
  where
    callSets = map callTiles (player ^. calls)

    segmentationsH :: [Tile] -> [SegmentedHand]
    segmentationsH [] = [[]]
    segmentationsH hand = pairSeg ++ tripleSeg ++ runSeg
      where
        sorted = sort hand

        recurseOn :: ([Tile] -> [(TileSet, [Tile])]) -> [SegmentedHand]
        recurseOn f = concatMap (\(set, rest) -> map (set :) (segmentationsH rest)) $ f sorted

        pairSeg = recurseOn (takeNOfAKind 2)
        tripleSeg = recurseOn (takeNOfAKind 3)
        runSeg = recurseOn takeRun

-- Some more helpers for possibleSegmentations that extracts the first of a given set from a hand
takeNOfAKind :: Int -> [Tile] -> [(TileSet, [Tile])]
takeNOfAKind n hand
  | length hand < n = []
  | otherwise = [splitAt n hand | all (== head hand) (take n hand)]

takeRun :: [Tile] -> [(TileSet, [Tile])]
takeRun (t1@(Number s r) : xs) =
  if r >= 8
    then []
    else do
      (t2, rest) <- findAndExtract (Number s (r + 1)) xs
      (t3, rest) <- findAndExtract (Number s (r + 2)) rest
      return ([t1, t2, t3], rest)
  where
    findAndExtract :: Tile -> [Tile] -> [(Tile, [Tile])]
    findAndExtract target xs = [(target, delete target xs) | target `elem` xs]
takeRun _ = []

containsSet :: PlayerState -> Tile -> TileSet -> Bool
containsSet player tile set = listContains set $ tile : (player ^. hand)

-- A closed hand is one where the player has made no open calls
handIsClosed :: PlayerState -> Bool
handIsClosed = not . any (\(Call _ ty) -> ty /= OpenKan) . view calls

tanyao :: Yaku
tanyao _ player extra
  | none isTerminal (extra : player ^. hand) = Just 1
  | otherwise = Nothing

yakuhaiSets :: (Direction, Direction) -> [TileSet]
yakuhaiSets (prevailing, seat) = winds ++ dragons
  where
    winds = [replicate 3 (Wind prevailing), replicate 3 (Wind seat)]
    dragons = [replicate 3 (Dragon col) | col <- [Red, White, Green]]

yakuhai :: Yaku
yakuhai gameState player extra =
  if any (containsSet player extra) (yakuhaiSets (gameState ^. prevailingWind, player ^. seatWind))
    then
      Just 1
    else
      Nothing

yakus :: [Yaku]
yakus = [tanyao, yakuhai]

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

playerMove :: Int -> GameState -> IO Move
playerMove player state = do
  let handStr = showHand $ state ^. players . element player . hand
  putStrLn $ "Your hand: " ++ handStr ++ " " ++ show (head (state ^. deck))
  print $ possibleSegmentations (state ^?! players . element player) (head (state ^. deck))
  getPlayerMove

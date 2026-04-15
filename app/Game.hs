{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Game where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List
import Tiles
import Utils

-- A call is a set of tiles the player has put on the table separate from their hand. A chii is a
-- run of 3 number tiles in sequence. Pon is 3 of a kind, and Kan is 4 of a kind. Kan can be open
-- or closed, depending on whether the any of the tiles were stolen from another player.
data Call = Call [Tile] CallType deriving (Show)

callTiles :: Call -> [Tile]
callTiles (Call tiles _) = tiles

data CallType = Pon | Chii | OpenKan | ClosedKan deriving (Eq, Show)

data PlayerState = PlayerState
  { _hand :: [Tile],
    _calls :: [Call]
  }
  deriving (Show)

-- The state of the game at any given point in time
data GameState = GameState
  { _deck :: Deck,
    _players :: [PlayerState]
  }
  deriving (Show)

-- Types of moves the player can make on their turn
-- Either the player can keep the new tile they picked up (discarding the old tile) or reject the
-- tile and not pick it up.
--
-- I will add kan and riichi later on
data Move = Keep Int | Reject deriving (Show)

makeLenses ''GameState
makeLenses ''PlayerState

newPlayer :: [Tile] -> PlayerState
newPlayer hand = PlayerState {_hand = hand, _calls = []}

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
  return GameState {_deck = deck, _players = newPlayer <$> hands}

-- A Yaku is a condition on a hand which gives a certain number of han when satisfied
-- the number of points may depend on the hand
--
-- If the function returns Nothing, the yaku is not satisfied. Otherwise returns the number of han
-- the function satisfies.
type Yaku = PlayerState -> Tile -> Maybe Int

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

-- A closed hand is one where the player has made no open calls
handIsClosed :: PlayerState -> Bool
handIsClosed = not . any (\(Call _ ty) -> ty /= OpenKan) . view calls

tanyao :: Yaku
tanyao player extra
  | none isTerminal (extra : player ^. hand) = Just 1
  | otherwise = Nothing

yakuhai :: Yaku
yakuhai player extra = undefined

yakus :: [Yaku]
yakus = [tanyao, yakuhai]

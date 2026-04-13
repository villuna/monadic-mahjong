module Tiles where

data Suit = Man | Pin | Sou deriving (Ord, Eq)

data Colour = Red | Green | White deriving (Ord, Eq)

data Direction = North | South | East | West deriving (Ord, Eq)

type Rank = Int

-- TODO red 5?
data Tile = Number Suit Rank | Dragon Colour | Wind Direction deriving (Ord, Eq)

type Hand = [Tile]

type Deck = [Tile]

isTerminal :: Tile -> Bool
isTerminal (Dragon _) = True
isTerminal (Wind _) = True
isTerminal (Number _ rank) = rank == 1 || rank == 9

-- A standard mahjong deck. Not in random order.
baseDeck :: Deck
baseDeck = concat $ replicate 4 quarterDeck

-- One of each type of mahjong tile; i.e., a quarter of a standard deck.
quarterDeck :: Deck
quarterDeck =
  [Number s r | s <- [Man, Pin, Sou], r <- [1 .. 9]]
    ++ (Dragon <$> [Red, Green, White])
    ++ (Wind <$> [East, North, West, South])

showHand :: Hand -> String
showHand = concatMap show

showWind :: Direction -> String
showWind dir = case dir of
  East -> "🀀"
  South -> "🀁"
  West -> "🀂"
  North -> "🀃"

showDragon :: Colour -> String
showDragon colour = case colour of
  Red -> "🀄"
  Green -> "🀅"
  White -> "🀆"

numberTileChars :: Suit -> [String]
numberTileChars Man = ["🀇", "🀈", "🀉", "🀊", "🀋", "🀌", "🀍", "🀎", "🀏"]
numberTileChars Sou = ["🀐", "🀑", "🀒", "🀓", "🀔", "🀕", "🀖", "🀗", "🀘"]
numberTileChars Pin = ["🀙", "🀚", "🀛", "🀜", "🀝", "🀞", "🀟", "🀠", "🀡"]

showNumberTile :: Suit -> Int -> String
showNumberTile suit rank = numberTileChars suit !! (rank - 1)

instance Show Tile where
  show (Wind dir) = showWind dir
  show (Dragon colour) = showDragon colour
  show (Number suit rank) = showNumberTile suit rank

module Utils where

import Control.Monad
import Data.Array
import Data.Array.IO
import Data.List
import System.Random

listContains :: (Eq a) => [a] -> [a] -> Bool
listContains [] _ = True
listContains (x : xs) ys = x `elem` ys && listContains xs (delete x ys)

-- Wrapper for listArray, since I will never need arrays that are not zero indexed by Ints
listToArray :: [a] -> Array Int a
listToArray lst = listArray (0, length lst - 1) lst

-- | Randomly shuffle a list
-- Taken from https://wiki.haskell.org/Random_shuffle
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
  ar <- newArray n xs
  forM [1 .. n] $ \i -> do
    j <- randomRIO (i, n)
    vi <- readArray ar i
    vj <- readArray ar j
    writeArray ar j vi
    return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1, n) xs

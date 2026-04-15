module Main where

import Game
import System.Exit
import Test.HUnit
import Tiles

testExtractPair :: Test
testExtractPair = TestCase (assertEqual "take pair" expected (takeNOfAKind 2 hand))
  where
    expected = [(replicate 2 (Wind East), replicate 12 (Number Man 1))]
    hand = replicate 2 (Wind East) ++ replicate 12 (Number Man 1)

testExtractRun :: Test
testExtractRun = TestCase (assertEqual "take run" expected (takeRun hand))
  where
    expected = [([Number Man 1, Number Man 2, Number Man 3], [Number Man 1, Number Man 2, Number Man 3] ++ replicate 8 (Wind East))]
    hand = replicate 2 (Number Man 1) ++ replicate 2 (Number Man 2) ++ replicate 2 (Number Man 3) ++ replicate 8 (Wind East)

testSegment :: Test
testSegment =
  TestCase (assertEqual "Hand with one non-overlapping segmentation" expected (possibleSegmentations state tile))
  where
    state =
      PlayerState
        { _calls = [],
          _hand = [Number s r | s <- [Man, Pin, Sou], r <- [1 .. 3]] ++ replicate 3 (Wind East) ++ [Number Man 5]
        }

    tile = Number Man 5

    expected = [[[Number Man 1, Number Man 2, Number Man 3]] ++ [replicate 2 (Number Man 5)] ++ [[Number s r | r <- [1 .. 3]] | s <- [Pin, Sou]] ++ [replicate 3 (Wind East)]]

main :: IO ()
main = do
  counts2 <- runTestTT (test [testSegment, testExtractPair, testExtractRun])
  if (errors counts2 + failures counts2 == 0)
    then exitSuccess
    else exitFailure

module Main where

import Test.Hspec
import Tests.Features.Spec (featureTests)
import Tests.Puzzles.Sudoku

main :: IO ()
main = hspec $ do
  featureTests
  describe "Puzzles" $ do
    sudokuTests

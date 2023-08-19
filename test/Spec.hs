module Main where

import Test.Hspec
import Tests.Features.Misc
import Tests.Puzzles.Sudoku

main :: IO ()
main = hspec $ do
  miscTests
  describe "Puzzles" $ do
    sudokuTests

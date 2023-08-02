module Main where

import Test.Hspec
import Sudoku
import Misc

main :: IO ()
main = hspec $ do
  miscTests
  sudokuTests


module Main where

import Test.Hspec
import Sudoku

main :: IO ()
main = hspec $ do
  sudokuTests


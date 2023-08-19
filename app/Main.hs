{-# OPTIONS_GHC -Wall #-}

module Main where

import PuzzleDSL.Text.Parser (puzzleParser)
import Text.Megaparsec

main :: IO ()
main = do
  let fname = "./app/Src.txt"
  src <- readFile fname
  parseTest puzzleParser src

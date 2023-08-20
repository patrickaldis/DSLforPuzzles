module PuzzleDSL.Text.Parser where

import Data.Void
import PuzzleDSL.Internal.Spec (PuzzleClass (..))
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

puzzleParser :: Parser String
puzzleParser = do
  space
  _ <- string "---"
  jsonBlock <- takeWhileP Nothing (/= '-')
  _ <- string "---"
  space
  ruleBlock <- takeWhileP Nothing (const True)
  pure (jsonBlock ++ " |||| " ++ ruleBlock)

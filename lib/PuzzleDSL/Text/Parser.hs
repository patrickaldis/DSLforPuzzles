module PuzzleDSL.Text.Parser where

import Data.Void
import PuzzleDSL.Internal.Spec (PuzzleClass (..))
import Text.Megaparsec

type Parser = Parsec Void String

puzzleParser :: Parser String
puzzleParser = pure "hey"

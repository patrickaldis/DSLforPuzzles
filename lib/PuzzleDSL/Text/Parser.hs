{-# LANGUAGE LambdaCase #-}

module PuzzleDSL.Text.Parser (puzzleParser) where

import Data.Map.Strict (fromList)
import Data.Void
import PuzzleDSL.Internal.Spec (CellEntrySet (Boolean, Numeric), CellPropertySet, CellType (..), PuzzleClass (..))
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

puzzleParser :: Parser String
puzzleParser = do
  space
  _ <- string "---"
  jsonBlock <- parseJsonBlock
  _ <- string "---"
  ruleBlock <- takeRest
  pure (show jsonBlock ++ " |||| " ++ ruleBlock)

parseJsonBlock :: Parser (String, [CellType])
parseJsonBlock = do
  nlIndent 0
  _ <- string "name:"
  hspace
  puzName <- word

  nlIndent 0
  _ <- string "cells:"
  hspace

  cs <- many parseCell

  nlIndent 0
  pure (puzName, cs)

parseCell :: Parser CellType
parseCell = do
  nlIndent 1
  cellName <- word
  _ <- char ':'

  nlIndent 2
  _ <- string "value:"
  hspace
  valueType <- parseDataType

  nlIndent 2
  _ <- string "properties:"
  ps <- many parseProperty
  let props = fromList ps
  pure
    ( CellType
        { typeName = cellName,
          values = valueType,
          propertySets = props
        }
    )

parseProperty :: Parser (String, CellPropertySet)
parseProperty = do
  nlIndent 3
  propName <- word
  _ <- char ':'
  hspace
  propType <- parseDataType
  pure (propName, propType)

parseDataType :: Parser CellPropertySet
parseDataType =
  choice [string "Int", string "Bool"] >>= \case
    "Bool" -> pure Boolean
    "Int" -> pure $ Numeric 9
    _ -> error "this cannot happen"

word :: Parser String
word = some alphaNumChar

nlIndent :: Int -> Parser ()
nlIndent n = hspace >> newline >> count n indent >> pure ()

indent :: Parser ()
indent = choice [skipCount 2 $ char ' ', skipCount 1 tab]

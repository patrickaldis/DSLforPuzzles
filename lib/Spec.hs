module Spec
  ( CellType (..),
    CellEntry (..),
    nValue,
    bValue,
    CellEntrySet (..),
    CellVar (..),
    SBoard,
    Expression (..),
    Rule (..),
    PuzzleClass (..),
    PuzzleInstance (..),
    PuzzleStructure,
    PuzzleState,
  )
where

import Data.SBV

data CellType = CellType
  { typeName :: String,
    possibleValues :: CellEntrySet
  }

data CellEntrySet  =
  Numeric Word8
  | Bool
  -- | FSet [String]

-- | A 2D array that identifies what cell is which type
type PuzzleStructure = [[CellType]]

data CellVar = CellVar
  { cellType :: CellType,
    value :: CellEntry,
    col :: Word8,
    row :: Word8
  }

data CellEntry
  = NumericEntry SWord8
  | BoolEntry SBool

instance Show CellEntry where
  show (NumericEntry n) = show n
  show (BoolEntry b) = show b
  -- | FSetEntry SString

nValue :: CellVar -> SWord8
nValue c = case value c of
  NumericEntry n -> n
  BoolEntry _ -> error $ errorMsg c (Numeric 0)

bValue :: CellVar -> SBool
bValue c = case value c of
  NumericEntry _ -> error $ errorMsg c Bool
  BoolEntry b -> b

errorMsg :: CellVar -> CellEntrySet -> String
errorMsg c set = unwords [
    "Error while casting types from variable: Expected",
    writeType . possibleValues . cellType $ c,
    "but got",
    writeType set
  ]
  where
    writeType :: CellEntrySet -> String
    writeType (Numeric _) = "Numeric"
    writeType Bool = "Bool"

type SBoard = [[CellVar]]

-- Puzzle Rules Type:
-- A type to define restrictions on the contents of the cells
data Rule
  = ForAll CellType (CellVar -> [Rule])
  | Constrain Expression

data Expression
  = Exp SBool
  | Count CellType Rule (Word8 -> Expression)

-- | A description of the layout and rules of a puzzle.
-- Comprises of `structure` and `constraints`
data PuzzleClass = PuzzleClass
  { name :: String,
    rules :: [Rule],
    types :: [CellType]
  }

-- Puzzle Instance Type
-- A partially solved puzzle, and the acompannying rules
type PuzzleState = [[Maybe CellEntry]]

-- | A partially filled out puzzle.
-- Contains the Problem description, and the state of the board
data PuzzleInstance = PuzzleInstance
  { puzzleclass :: PuzzleClass,
    structure :: PuzzleStructure,
    state :: PuzzleState
  }

module Spec
  ( CellType (..),
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
  { cellName :: String,
    noValues :: Word8
  }

-- | A 2D array that identifies what cell is which type
type PuzzleStructure = [[CellType]]

data CellVar = CellVar
  { cellType :: CellType,
    value :: SWord8,
    col :: Word8,
    row :: Word8
  }

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
type PuzzleState = [[Maybe Word8]]

-- | A partially filled out puzzle.
-- Contains the Problem description, and the state of the board
data PuzzleInstance = PuzzleInstance
  { puzzleclass :: PuzzleClass,
    structure :: PuzzleStructure,
    state :: PuzzleState
  }

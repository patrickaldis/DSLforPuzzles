module Spec2 where

import Data.SBV

type Symbol = Int

-- Puzzle Structure Type:
-- A type identifying which cell is what type
data CellType = CellType -- {{{
  { cellName :: String,
    noValues :: Word8
  }

data CellVar = CellVar
  { cellType :: CellType,
    value :: SWord8,
    col :: Word8,
    row :: Word8
  }

type SBoard = [[CellVar]]

-- | A 2D array that identifies what cell is which type
type PuzzleStructure = [[CellType]] -- }}}

-- Puzzle Rules Type:
-- A type to define restrictions on the contents of the cells
data Constraint -- {{{
  = ForAll CellType (CellVar -> [Constraint])
  | Exp SBool

-- | A description of the layout and rules of a puzzle.
-- Comprises of `structure` and `constraints`
data Problem = Problem
  { name :: String,
    structure :: PuzzleStructure,
    constraints :: [Constraint]
  } -- }}}

-- Puzzle Instance Type
-- A partially solved puzzle, and the acompannying rules
type PuzzleState = [[Maybe Word8]] -- {{{

-- | A partially filled out puzzle.
-- Contains the Problem description, and the state of the board
data PuzzleInstance = PuzzleInstance
  { problem :: Problem,
    state :: PuzzleState
  } -- }}}

{-

>>> 1 + 2

-}
-- data Cell a = Cell
--   { x :: Int,
--     y :: Int,
--     value :: a
--   }
--     | Cell2 Int
--
-- type Board a = [[Maybe (Cell a)]]

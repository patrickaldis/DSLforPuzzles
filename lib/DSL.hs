module DSL
  ( col,
    row,
    Constraint (..),
    Problem (..),
    CellType (..),
    PuzzleInstance (..),

    solveAll,
    printSols,
  )
where

import Data.SBV
import Spec hiding (col, row)
import qualified Spec

import Solve

col :: CellVar -> SWord8
col = literal . Spec.col

row :: CellVar -> SWord8
row = literal . Spec.row

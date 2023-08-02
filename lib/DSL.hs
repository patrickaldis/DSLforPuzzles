module DSL
  ( col,
    row,
    value,
    Rule (..),
    Expression (..),
    CellType (..),
    PuzzleInstance (..),
    PuzzleClass (..),
    div,
    (.&&),
    (.==),
    (./=),
    (.=>),

    solveAll,
    printSols,
  )
where

import Prelude hiding (div)

import Data.SBV
import Spec hiding (col, row)
import qualified Spec

import Solve

col :: CellVar -> SWord8
col = literal . Spec.col

row :: CellVar -> SWord8
row = literal . Spec.row

div :: SWord8 -> SWord8 -> SWord8
div = sDiv

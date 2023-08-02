module DSL
  ( col,
    row,
    nValue,
    bValue,
    Rule (..),
    Expression (..),
    CellType (..),
    CellEntry (..),
    CellEntrySet (..),
    PuzzleInstance (..),
    PuzzleClass (..),
    div,
    literal,
    (.&&),
    (.==),
    (./=),
    (.=>),
    solveAll,
    printSols,
  )
where

import Data.SBV
import Solve
import Spec hiding (col, row)
import qualified Spec
import Prelude hiding (div)

col :: CellVar -> SWord8
col = literal . Spec.col

row :: CellVar -> SWord8
row = literal . Spec.row

div :: SWord8 -> SWord8 -> SWord8
div = sDiv


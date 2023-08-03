module DSL
  ( nVal,
    bVal,
    nProp,
    bProp,
    row,
    col,
    Rule (..),
    Expression (..),
    CellType (..),
    CellEntry (..),
    CellEntrySet (..),
    CellProperty (..),
    CellPropertySet,
    PuzzleInstance (..),
    PuzzleClass (..),
    PuzzleSolution,
    CellState (..),
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
import Spec
import Utils
import Prelude hiding (div)

div :: SWord8 -> SWord8 -> SWord8
div = sDiv


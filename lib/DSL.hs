-- |
-- Module : DSL
-- Contains all functions necessary to describe a puzzle in the language
--
-- The intention is that all a user has to do is write:
-- import DSL
--
-- And have everything they need
module DSL
  ( nVal,
    bVal,
    nProp,
    bProp,
    row,
    col,
    Utils.lookup,
    Rule (..),
    Expression (..),
    BinarizeRule (..),
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
    solve,
    printSols,
  )
where

import Data.SBV hiding (solve)
import Solve
import Spec
import Utils
import Prelude hiding (div)

div :: SWord8 -> SWord8 -> SWord8
div = sDiv

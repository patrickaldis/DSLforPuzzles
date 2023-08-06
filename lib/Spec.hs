module Spec
  ( CellType (..),
    CellEntry (..),
    CellProperty (..),
    CellEntrySet (..),
    CellPropertySet,
    CellVar (..),
    Index,
    SBoard,
    Expression (..),
    Rule (..),
    ComponentMap,
    BinarizeRule (..),
    PuzzleClass (..),
    PuzzleInstance (..),
    PuzzleStructure,
    PuzzleState,
    CellState (..),
  )
where

import Data.Map.Strict
import Data.SBV

-- Puzzle Rules Type:
-- A type to define restrictions on the contents of the cells
data Rule
  = ForAll CellType (CellVar -> [Rule])
  | Constrain Expression
  | CountComponents [BinarizeRule] (ComponentMap -> Rule)

data Expression
  = Exp SBool
  | If Expression Expression
  | Count CellType Rule (SWord8 -> Expression)
  | ConnectedBy CellVar CellVar ComponentMap (SBool -> Expression)

data BinarizeRule
  = For CellType (CellVar -> Expression)

-- | A description of the layout and rules of a puzzle.
-- Comprises of `structure` and `constraints`
data PuzzleClass = PuzzleClass
  { name :: String,
    rules :: [Rule],
    types :: [CellType]
  }

-- Puzzle Instance Type
-- A partially solved puzzle, and the acompannying rules
type PuzzleState = [[CellState]]

data CellState = CellState
  { valueState :: Maybe CellEntry,
    propertyStates :: Map String CellProperty
  }

data CellType = CellType
  { typeName :: String,
    values :: CellEntrySet,
    propertySets :: Map String CellPropertySet
  }

data CellEntrySet
  = Numeric Word8
  | Bool

-- \| FSet [String]

-- | A 2D array that identifies what cell is which type
type PuzzleStructure = [[CellType]]

data CellVar = CellVar
  { cellType :: CellType,
    value :: CellEntry,
    properties :: Map String CellProperty
  }

type Index = (Word8, Word8)

type ComponentMap = [[SWord8]]

data CellEntry
  = NumericEntry SWord8
  | BoolEntry SBool

data CellProperty
  = NumericProp Word8
  | BoolProp Bool

type CellPropertySet = CellEntrySet

instance Show CellEntry where
  show (NumericEntry n) = show n
  show (BoolEntry b) = show b

type SBoard = [[CellVar]]

-- | A partially filled out puzzle.
-- Contains the Problem description, and the state of the board
data PuzzleInstance = PuzzleInstance
  { puzzleclass :: PuzzleClass,
    structure :: PuzzleStructure,
    state :: PuzzleState
  }

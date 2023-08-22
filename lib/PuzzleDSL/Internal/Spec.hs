-- |
-- Module: Spec
-- Specifies all the datatypes used in the DSL
module PuzzleDSL.Internal.Spec
  ( -- DSL Expressions Objects
    Rule (..),
    Expression (..),
    Expressable (..),
    CellRule,
    CellTypeRule (..),
    ComponentMap,
    -- Variables
    CellVar (..),
    -- Puzzle Boilerplate
    PuzzleClass (..),
    PuzzleInstance (..),
    PuzzleStructure,
    PuzzleState,
    CellState (..),
    CellType (..),
    CellEntry (..),
    CellEntrySet (..),
    CellProperty (..),
    CellPropertySet,
    Index,
    SBoard,
    emptyType,
  )
where

import Data.Map.Strict
import Data.SBV

-- | Base datatype for expressing rules in the DSL
data Rule
  = -- | `ForAll` constructor iterates over all cells of a given type
    ForAll CellType (CellVar -> [Rule])
  | -- | `Constrain` enforces constraints on the variables in scope
    Constrain (Expression Bool)

-- | Datatype for expressing term-level expresions. In an
-- `Expression`, all variables are in scope
data Expression a
  = Exp (SBV a)
  | If (Expression Bool) (Expression a)
  | Count (CellRule Bool) (SWord8 -> Expression a)
  | Sum (CellRule Word8) (SWord8 -> Expression a)
  | ConnectedBy (CellRule Bool) CellVar CellVar (SBool -> Expression a)

class SymVal a => Expressable a where
  identity :: (SBV a)

instance Expressable Bool where
  identity = sTrue

instance Expressable Word8 where
  identity = literal 0

type CellRule a = [CellTypeRule a]

data CellTypeRule a
  = For CellType (CellVar -> Expression a)

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
  deriving (Show)

emptyType :: CellType
emptyType =
  CellType
    { typeName = "emptyType",
      values = Numeric 0,
      propertySets = empty
    }

data CellEntrySet
  = Numeric Word8
  | Boolean
  deriving (Show)

-- \| FSet [String]

-- | A 2D array that identifies what cell is which type
type PuzzleStructure = [[CellType]]

-- | ADT used to represent a cell in a puzzle.
-- Each cell in the puzzle contains:
-- - A celltype: defines the type and range of the cell value.
-- - A value: the value itself
-- - Properties: additional values added specific to the puzzle
data CellVar = CellVar
  { cellType :: CellType,
    value :: CellEntry,
    properties :: Map String CellProperty
  }

type Index = (Word8, Word8)

type ComponentMap = [[SBool]]

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

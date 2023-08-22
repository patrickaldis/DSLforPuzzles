-- | Generally contains DSL combinators,
-- Functions are listed here, rather than directly in `DSL`
-- such as not to create a cyclic dependency, as they are likely
-- used in `Solve` also
module PuzzleDSL.Internal.Utils where

import Data.Map.Strict ((!))
import Data.Maybe (fromJust)
import Data.SBV
import PuzzleDSL.Internal.Spec
import Prelude hiding (lookup)

-- | Unwraps a numeric cell value from a `CellVar`
-- If the cell doesn't have a numeric value
-- it throws an error.
nVal :: CellVar -> SWord8
nVal = castToNum . value

-- | Unwraps a boolean cell value from a `CellVar`
-- If the cell doesn't have a boolean value
-- it throws an error.
bVal :: CellVar -> SBool
bVal = castToBool . value

-- | Unwraps a named numeric property from a `CellVar`
-- If the cell doesn't have the named numeric property
-- it throws an error.
nProp :: String -> CellVar -> SWord8
nProp pname = literal . castPropToNum . (! pname) . properties

-- | Provided only for `rawCol`/`rawRow`
rawNProp :: String -> CellVar -> Word8
rawNProp pname = castPropToNum . (! pname) . properties

-- | Unwraps a named boolean property from a `CellVar`
-- If the cell doesn't have the named boolean property
-- it throws an error.
bProp :: String -> CellVar -> SBool
bProp pname = literal . castPropToBool . (! pname) . properties

-- | Given a 1-based index of the form (Word8, Word8), looks up
-- the value at that index in the array. Used in `Component`
lookup :: Index -> [[a]] -> a
lookup ix a = let (i, j) = fromEnum <$$> ix in a !! (i - 1) !! (j - 1)

-- | `bimap` f f from bifunctors. Applies f to both arguments of the tuple
(<$$>) :: (a -> b) -> (a, a) -> (b, b)
(<$$>) f (x, y) = (f x, f y)

-- | looks up the row and column of a cellvar, and extracts the
-- corresponding element in the array
lookupVar :: CellVar -> [[a]] -> a
lookupVar x vs = let ix = (rawRow x + 1, rawCol x + 1) in lookup ix vs

-- | The column of a cell in the grid
col :: CellVar -> SWord8
col = nProp "col"

-- | The row of a cell in the grid
row :: CellVar -> SWord8
row = nProp "row"

-- | The column of a cell in the grid, as a pure, non symbolic
-- `Word8` type
rawCol :: CellVar -> Word8
rawCol = fromJust . unliteral . literal . rawNProp "col"

-- | The row of a cell in the grid, as a pure, non symbolic
-- `Word8` type
rawRow :: CellVar -> Word8
rawRow = fromJust . unliteral . literal . rawNProp "row"

-- | Applies a monadic action to every element in a nested list
mapM2d :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
mapM2d f = mapM (mapM f)

-- | `map`, for nested lists.
map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

-- | `zip`, for nested lists.
zip2d :: [[a]] -> [[b]] -> [[(a, b)]]
zip2d = zipWith zip

-- | Given two nested lists, pair up matching indices
-- to create a new nested list, with a binary function
zipMapM2d :: Monad m => (a -> b1 -> m b2) -> [[a]] -> [[b1]] -> m [[b2]]
zipMapM2d f s = mapM2d (uncurry f) . zip2d s

-- Internal functions to generate error messages___________

castToBool :: CellEntry -> SBool
castToBool (BoolEntry b) = b
castToBool c = error $ errorMsg c Boolean

castToNum :: CellEntry -> SWord8
castToNum (NumericEntry n) = n
castToNum c = error $ errorMsg c Boolean

castPropToBool :: CellProperty -> Bool
castPropToBool (BoolProp b) = b
castPropToBool c = error $ errorMsg c Boolean

castPropToNum :: CellProperty -> Word8
castPropToNum (NumericProp n) = n
castPropToNum c = error $ errorMsg c Boolean

class HasCellType a where
  printType :: a -> String

instance HasCellType CellEntrySet where
  printType (Numeric _) = "Numeric"
  printType Boolean = "Boolean"

instance HasCellType CellEntry where
  printType (NumericEntry _) = "Numeric"
  printType (BoolEntry _) = "Boolean"

instance HasCellType CellProperty where
  printType (NumericProp _) = "Numeric"
  printType (BoolProp _) = "Boolean"

instance HasCellType CellVar where
  printType = printType . value

errorMsg :: (HasCellType a, HasCellType b) => a -> b -> String
errorMsg val target =
  unwords
    [ "Error while retrieving types from variable: Expected",
      printType target,
      "but got",
      printType val
    ]

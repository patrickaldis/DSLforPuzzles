module Utils where

import Data.Map.Strict (empty, (!))
import Data.Maybe (fromJust)
import Data.SBV
import Spec
import Prelude hiding (lookup)

nVal :: CellVar -> SWord8
nVal = castToNum . value

bVal :: CellVar -> SBool
bVal = castToBool . value

nProp :: String -> CellVar -> SWord8
nProp pname = literal . castPropToNum . (! pname) . properties

rawNProp :: String -> CellVar -> Word8
rawNProp pname = castPropToNum . (! pname) . properties

bProp :: String -> CellVar -> SBool
bProp pname = literal . castPropToBool . (! pname) . properties

lookup :: Index -> [[a]] -> a
lookup ix a = let (i, j) = fromEnum <$$> ix in a !! (i - 1) !! (j - 1)

(<$$>) :: (a -> b) -> (a, a) -> (b, b)
(<$$>) f (x, y) = (f x, f y)

lookupVar :: CellVar -> [[a]] -> a
lookupVar x vs = let ix = (rawRow x + 1, rawCol x + 1) in lookup ix vs

col :: CellVar -> SWord8
col = nProp "col"

row :: CellVar -> SWord8
row = nProp "row"

rawCol :: CellVar -> Word8
rawCol = fromJust . unliteral . literal . rawNProp "col"

rawRow :: CellVar -> Word8
rawRow = fromJust . unliteral . literal . rawNProp "row"

mapM2d :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
mapM2d f = mapM (mapM f)

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

zip2d :: [[a]] -> [[b]] -> [[(a, b)]]
zip2d = zipWith zip

zipMapM2d :: Monad m => (a -> b1 -> m b2) -> [[a]] -> [[b1]] -> m [[b2]]
zipMapM2d f s = mapM2d (uncurry f) . zip2d s

emptyType :: CellType
emptyType =
  CellType
    { typeName = "emptyType",
      values = Numeric 0,
      propertySets = empty
    }

castToBool :: CellEntry -> SBool
castToBool (BoolEntry b) = b
castToBool c = error $ errorMsg c Bool

castToNum :: CellEntry -> SWord8
castToNum (NumericEntry n) = n
castToNum c = error $ errorMsg c Bool

castPropToBool :: CellProperty -> Bool
castPropToBool (BoolProp b) = b
castPropToBool c = error $ errorMsg c Bool

castPropToNum :: CellProperty -> Word8
castPropToNum (NumericProp n) = n
castPropToNum c = error $ errorMsg c Bool

class HasCellType a where
  printType :: a -> String

instance HasCellType CellEntrySet where
  printType (Numeric _) = "Numeric"
  printType Bool = "Bool"

instance HasCellType CellEntry where
  printType (NumericEntry _) = "Numeric"
  printType (BoolEntry _) = "Bool"

instance HasCellType CellProperty where
  printType (NumericProp _) = "Numeric"
  printType (BoolProp _) = "Bool"

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

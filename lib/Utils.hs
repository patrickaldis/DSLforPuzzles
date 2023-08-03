module Utils where

import Spec
import Data.SBV
import Data.Map.Strict ((!))
import Data.Maybe (fromJust)

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

col :: CellVar -> SWord8
col = nProp "col"

row :: CellVar -> SWord8
row = nProp "row"

rawCol :: CellVar -> Word8
rawCol = fromJust . unliteral . literal . rawNProp "col"

rawRow ::CellVar -> Word8
rawRow = fromJust . unliteral . literal . rawNProp "row"

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

errorMsg :: (HasCellType a , HasCellType b) => a -> b -> String
errorMsg val target = unwords [
    "Error while retrieving types from variable: Expected",
    printType target,
    "but got",
    printType val
  ]


module DSL where

import Data.SBV
import Spec2

col :: CellVar -> SWord8
col = literal . Spec2.col

row :: CellVar -> SWord8
row = literal . Spec2.row

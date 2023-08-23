module PuzzleDSL.Internal.Features.Count (toSumRule) where

import Data.SBV
import PuzzleDSL.Internal.Spec (CellRule, CellTypeRule (..), Expression (..))

toSumRule :: CellRule Bool -> CellRule Word8
toSumRule = map f
  where
    f :: CellTypeRule Bool -> CellTypeRule Word8
    f (For t fExp) = For t (toSumExp . fExp)

toSumExp :: Expression Bool -> Expression Word8
toSumExp (Exp b) = Exp $ ite b 1 0
toSumExp (If cond expr) = If cond $ toSumExp expr
toSumExp (Count rule fExpr) = Count rule (toSumExp . fExpr)
toSumExp (Sum rule fExpr) = Sum rule (toSumExp . fExpr)
toSumExp (ConnectedBy x y cmap fExpr) = ConnectedBy x y cmap (toSumExp . fExpr)

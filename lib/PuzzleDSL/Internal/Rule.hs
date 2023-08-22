-- |
-- Module: Rule
-- Contains functions for parsing rules to produce `sbv` expressions
module PuzzleDSL.Internal.Rule (applyRules) where

import Control.Monad (forM)
import Data.SBV
import PuzzleDSL.Internal.Features.Component
import PuzzleDSL.Internal.Features.Sum
import PuzzleDSL.Internal.Spec
import PuzzleDSL.Internal.Utils

-- | Evaluate an expression to produce a result
applyExpr :: Expressable a => Expression a -> SBoard -> SBV a
applyExpr (Exp expr) _ = expr
applyExpr (If cond expr) b = ite (applyExpr cond b) (applyExpr expr b) identity
applyExpr (Count rule fExpr) b = applyExpr (fExpr 0) b
applyExpr (Sum rule fExpr) b =
  let total = cellSum appliedRule
      appliedRule = applyCellRule rule b
   in applyExpr (fExpr total) b
applyExpr (ConnectedBy x y cmap fExpr) b = applyExpr (fExpr sFalse) b

-- | Takes a list of rules and enforce them on the supplied grid
-- `cs cellTypes bList board`
-- - *cellTypes* annotation of the cell types
-- - *bList* blacklist of all cells currently in scope,
--           so they cannot be introduced by new ForAll
--           bindings
-- - *board* the board to apply the rules to
applyRules :: [Rule] -> PuzzleStructure -> [(Word8, Word8)] -> SBoard -> Symbolic SBool
applyRules cs cellTypes bList board =
  sAnd
    <$> mapM
      (\c -> applyRule c cellTypes bList board)
      cs

-- | Applies a single rule to the board
applyRule :: Rule -> PuzzleStructure -> [(Word8, Word8)] -> SBoard -> Symbolic SBool
applyRule (ForAll xType fExpr) ts bList board =
  sAnd
    <$> forM
      xs
      (\x -> applyRules (fExpr x) ts (newbList x) board)
  where
    xs = [x | xs' <- zip2d board ts, (x, t) <- xs', f x t]
    f x t =
      typeName t == typeName xType
        && coords x `notElem` bList
    coords x = (rawRow x, rawCol x)
    newbList x = coords x : bList
applyRule (Constrain expr) _ _ b = return $ applyExpr expr b

-- | Function for applying a per cell rule to the board
applyCellRule :: Expressable a => CellRule a -> SBoard -> [[SBV a]]
applyCellRule rs' b = map2d (f rs') b
  where
    f :: Expressable a => CellRule a -> CellVar -> SBV a
    f rs v =
      let tName = typeName . cellType $ v
          r = case filter (\(For t _) -> typeName t == tName) rs of
            [] -> For emptyType (\_ -> Exp identity)
            (x : _) -> x
       in g r v
    g :: Expressable a => CellTypeRule a -> CellVar -> SBV a
    g (For _ fExpr) v = applyExpr (fExpr v) b

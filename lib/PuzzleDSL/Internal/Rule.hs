-- |
-- Module: Rule
-- Contains functions for parsing rules to produce `sbv` expressions
module PuzzleDSL.Internal.Rule (applyRules) where

import Control.Monad (forM)
import Data.SBV
import PuzzleDSL.Internal.Component
import PuzzleDSL.Internal.Spec
import PuzzleDSL.Internal.Utils

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
applyRule (Constrain expr) _ _ _ = return $ applyExpr expr
applyRule (CountComponents binFunc fRule) ts bList board =
  let binBoard = binarize binFunc board
   in applyRule (fRule binBoard) ts bList board

-- | Function for applying a `BinarizeRule` to the board
binarize :: [BinarizeRule] -> SBoard -> [[SBool]]
binarize rs' = map2d (f rs')
  where
    f :: [BinarizeRule] -> CellVar -> SBool
    f rs v =
      let tName = typeName . cellType $ v
          r = case filter (\(For t _) -> typeName t == tName) rs of
            [] -> For emptyType (\_ -> Exp sTrue)
            (x : _) -> x
       in g r v
    g :: BinarizeRule -> CellVar -> SBool
    g (For _ fExpr) v = applyExpr $ fExpr v

-- | Evaluate an expression to produce an `SBool`
applyExpr :: Expression -> SBool
applyExpr (Exp expr) = expr
applyExpr (If cond expr) = applyExpr cond .=> applyExpr expr
applyExpr (Count _ _ fExpr) = applyExpr (fExpr 0)
applyExpr (ConnectedBy x y cmap fExpr) =
  let a <~> b = compRelation cmap a b
   in applyExpr (fExpr $ x <~> y)

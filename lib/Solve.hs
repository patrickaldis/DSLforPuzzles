{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Solve
  ( asPredicate,
    solveAll,
    solve,
    printSols,
    PuzzleSolution,
  )
where

import Component hiding (neighbors)
import Control.Monad (forM, forM_)
import Data.Map.Strict hiding (filter, map)
import Data.Maybe (fromJust)
import Data.SBV hiding (solve)
import Data.SBV.Internals (genFromCV)
import Spec
import Utils hiding (lookup)
import Prelude hiding (lookup)

-- | Creates an SBV predicate describing the problem,
-- from a `PuzzleInstance`
asPredicate :: PuzzleInstance -> Predicate
asPredicate inst =
  let -- Unwrap PuzzleInstance
      states = state inst
      struct = structure inst
      pclass = puzzleclass inst
      consts = rules pclass
   in do
        -- Create a bunch of free variables that constitute a board
        emptyBoard struct
        >>= writeProps states
        >>= writeValues states
        >>= applyRules consts struct []

type PuzzleSolution = [[Word8]]

-- Convenience Functions{{{
--

-- | Fetches all solutions to the PuzzleInstance from the solver,
-- Giving the result as a grid of values
solveAll :: PuzzleInstance -> IO [PuzzleSolution]
solveAll inst = do
  sols <- allSatWith z3 $ asPredicate inst
  let ts = structure inst
      dictionaries = getModelDictionaries sols
      arrays = do
        d <- dictionaries
        let f :: String -> Word8
            f q =
              fromJust
                ( do
                    modelValue <- lookup q d
                    Just $ genFromCV modelValue
                )

        return $ map (map f) $ varNames ts
  return arrays

solve :: PuzzleInstance -> IO (Maybe PuzzleSolution)
solve x = do
  sols <- solveAll x
  return
    ( case sols of
        (x' : _) -> Just x'
        _ -> Nothing
    )

-- | Pretty prints all solutions to the puzzle,
-- with the original problem shown above
printSols :: Maybe PuzzleInstance -> [PuzzleSolution] -> IO ()
printSols initial sols = do
  case initial of
    Just iProb -> do
      putStrLn "Initial Problem:"
      printProb iProb
    Nothing -> return ()

  forM_
    (zip sols [1 ..])
    ( \(p, i) ->
        putStrLn ("Solution " ++ show i ++ ":")
          >> printSol p
    )
  where
    printProb :: PuzzleInstance -> IO ()
    printProb prob =
      putStrLn . unlines $
        ( do
            rows <- state prob
            return $
              unwords
                ( do
                    cell <- rows
                    case valueState cell of
                      Just n -> return $ show n
                      Nothing -> return "?"
                )
        )
    printSol :: PuzzleSolution -> IO ()
    printSol sol = putStrLn . unlines $ [unwords [show c | c <- rows] | rows <- sol]

-- }}}

-- | Creates an empty array of symbolic variables, where each{{{
-- variables takes values according to PuzzleStructure
emptyBoard :: PuzzleStructure -> Symbolic SBoard
emptyBoard ts = mapM2d (uncurry initialiseVar) (zip2d ts indices)
  where
    initialiseVar :: CellType -> (Word8, Word8) -> Symbolic CellVar
    initialiseVar t (i, j) = do
      let varName = "X" ++ show i ++ ":" ++ show j
          cell v =
            CellVar
              { cellType = t,
                value = v,
                properties =
                  fromList
                    [ ("col", NumericProp i),
                      ("row", NumericProp j)
                    ]
              }
      case values t of
        Numeric n -> do
          v <- free varName :: Symbolic SWord8
          constrain $ v .>= 1 .&& v .<= literal n
          return . cell $ NumericEntry v
        Bool -> do
          b <- free varName :: Symbolic SBool
          return . cell $ BoolEntry b

-- }}}

varNames :: PuzzleStructure -> [[String]]
varNames = map2d fst . zip2d varNamesInf
  where
    varNamesInf = map2d (\(i, j) -> "X" ++ show i ++ ":" ++ show j) indices

indices :: [[(Word8, Word8)]]
indices = [[(i, j) | j <- [0 ..]] | i <- [0 ..]]

writeValues :: PuzzleState -> SBoard -> Symbolic SBoard -- {{{
writeValues = zipMapM2d f
  where
    f :: CellState -> CellVar -> Symbolic CellVar
    f s x = do
      case valueState s of
        (Just (NumericEntry n)) -> constrain (nVal x .== n)
        (Just (BoolEntry b)) -> constrain (bVal x .== b)
        Nothing -> constrain sTrue
      return x

writeProps :: PuzzleState -> SBoard -> Symbolic SBoard
writeProps = zipMapM2d f
  where
    f :: CellState -> CellVar -> Symbolic CellVar
    f s x =
      return
        ( x
            { properties = properties x `union` propertyStates s
            }
        )

-- }}}
applyRules :: [Rule] -> PuzzleStructure -> [(Word8, Word8)] -> SBoard -> Symbolic SBool
applyRules cs cellTypes bList board =
  sAnd
    <$> mapM
      (\c -> applyRule c cellTypes bList board)
      cs

applyRule :: Rule -> PuzzleStructure -> [(Word8, Word8)] -> SBoard -> Symbolic SBool
applyRule (ForAll xType fExpr) ts bList board =
  let xs = [x | xs' <- zip2d board ts, (x, t) <- xs', f x t]
      f x t =
        typeName t == typeName xType
          && coords x `notElem` bList

      newbList x = coords x : bList
   in sAnd <$> forM xs (\x -> applyRules (fExpr x) ts (newbList x) board)
  where
    coords x = (rawRow x, rawCol x)
applyRule (Constrain expr) _ _ _ = return $ applyExpr expr
applyRule (CountComponents binFunc fRule) ts bList board =
  let binBoard = binarize binFunc board
      cmap = components binBoard
   in applyRule (fRule cmap) ts bList board

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

applyExpr :: Expression -> SBool
applyExpr (Exp expr) = expr
applyExpr (If cond expr) = applyExpr cond .=> applyExpr expr
applyExpr (Count _ _ fExpr) = applyExpr (fExpr 0)
applyExpr (ConnectedBy x y cmap fExpr) =
  let cmpX = lookupVar x cmap
      cmpY = lookupVar y cmap
   in applyExpr (fExpr $ cmpX .== cmpY)

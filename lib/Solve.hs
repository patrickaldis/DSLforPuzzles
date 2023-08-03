{-# OPTIONS_GHC -Wno-type-defaults #-}

module Solve
  ( asPredicate,
    solveAll,
    printSols,
    PuzzleSolution
  )
where

import Control.Monad (forM, forM_)
import Data.Map.Strict hiding (map)
import Data.Maybe (fromJust)
import Data.SBV
import Data.SBV.Internals (genFromCV)
import Spec
import Prelude hiding (lookup)
import Utils

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
  sols <- allSat $ asPredicate inst
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
                properties = fromList [
                  ("col", NumericProp i),
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
    f s x = return (x {
      properties = properties x `union` propertyStates s
    })

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
applyRule (Constrain expr) _ _ _ = applyExpr expr

applyExpr :: Expression -> Symbolic SBool
applyExpr (Exp expr) = return expr
applyExpr (Count _ _ fExpr) = applyExpr (fExpr 0)

mapM2d :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
mapM2d f = mapM (mapM f)

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

zip2d :: [[a]] -> [[b]] -> [[(a, b)]]
zip2d = zipWith zip

zipMapM2d :: Monad m => (a -> b1 -> m b2) -> [[a]] -> [[b1]] -> m [[b2]]
zipMapM2d f s = mapM2d (uncurry f) . zip2d s
-- convert :: Rule -> Predicate
-- convert = convert' ["X" ++ show n | n <- [1 ..]]
--
-- convert' :: [String] -> Rule -> Predicate
-- convert' (l : ls) (ForAll _ f) = do
--   x <- free l :: Symbolic SInteger
--   let cs = f x :: [Rule]
--   mapM_ (convert' ls) cs
--   return sTrue
-- convert' _ (Exp e) = constrain e >> return sTrue
-- convert' _ _ = return sTrue

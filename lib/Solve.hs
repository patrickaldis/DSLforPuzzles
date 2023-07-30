{-# OPTIONS_GHC -Wno-type-defaults #-}

module Solve (asPredicate, solveAll, printSols) where

import Control.Monad (forM, forM_)
import Data.Map.Strict hiding (map)
import Data.Maybe (fromJust)
import Data.SBV
import Data.SBV.Internals (genFromCV)
import Spec
import Prelude hiding (lookup)

-- | Creates an SBV predicate describing the problem,
-- from a `PuzzleInstance`
asPredicate :: PuzzleInstance -> Predicate
asPredicate inst =
  let -- Unwrap PuzzleInstance
      states = state inst
      p = problem inst

      cellTypes = structure p
      consts = constraints p
   in do
        -- Create a bunch of free variables that constitute a board
        emptyBoard cellTypes
        >>= writeLiterals states
        >>= applyConstraints consts cellTypes []

type PuzzleSolution = [[Word8]]

-- Convenience Functions{{{
--

-- | Fetches all solutions to the PuzzleInstance from the solver,
-- Giving the result as a grid of values
solveAll :: PuzzleInstance -> IO [PuzzleSolution]
solveAll inst = do
  sols <- allSat $ asPredicate inst
  let ts = structure $ problem inst
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
                    case cell of
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
      v <- free $ "X" ++ show i ++ ":" ++ show j :: Symbolic SWord8
      constrain $ v .> 0 .&& v .< literal (noValues t + 1)
      let cell =
            CellVar
              { cellType = t,
                value = v,
                col = j,
                row = i
              }
      return cell

-- }}}

varNames :: PuzzleStructure -> [[String]]
varNames = map2d fst . zip2d varNamesInf
  where
    varNamesInf = map2d (\(i, j) -> "X" ++ show i ++ ":" ++ show j) indices

indices :: [[(Word8, Word8)]]
indices = [[(i, j) | j <- [0 ..]] | i <- [0 ..]]

writeLiterals :: PuzzleState -> SBoard -> Symbolic SBoard -- {{{
writeLiterals s = mapM2d (uncurry f) . zip2d s
  where
    f :: Maybe Word8 -> CellVar -> Symbolic CellVar
    f (Just n) x = constrain (value x .== literal n) >> return x
    f Nothing x = return x

-- }}}

applyConstraints ::
  [Constraint] ->
  PuzzleStructure ->
  [(Word8, Word8)] ->
  SBoard ->
  Symbolic SBool
applyConstraints cs cellTypes bList board =
  sAnd
    <$> mapM
      (\c -> applyConstraint c cellTypes bList board)
      cs
  where
    applyConstraint :: Constraint -> PuzzleStructure -> [(Word8, Word8)] -> SBoard -> Symbolic SBool
    applyConstraint (ForAll xType fExpr) ts bList board =
      let xs = do
            xs' <- zip2d board ts
            (x, t) <- xs'
            if cellName t == cellName xType && (not . elem (row x, col x) $ bList)
              then [x]
              else []

          newbList x = (row x, col x) : bList
       in sAnd <$> forM xs (\x -> applyConstraints (fExpr x) ts (newbList x) board)
    applyConstraint (Exp expr) _ _ _ = return expr

mapM2d :: Monad m => (a -> m b) -> [[a]] -> m [[b]]
mapM2d f = mapM (mapM f)

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

zip2d :: [[a]] -> [[b]] -> [[(a, b)]]
zip2d = zipWith zip

-- convert :: Constraint -> Predicate
-- convert = convert' ["X" ++ show n | n <- [1 ..]]
--
-- convert' :: [String] -> Constraint -> Predicate
-- convert' (l : ls) (ForAll _ f) = do
--   x <- free l :: Symbolic SInteger
--   let cs = f x :: [Constraint]
--   mapM_ (convert' ls) cs
--   return sTrue
-- convert' _ (Exp e) = constrain e >> return sTrue
-- convert' _ _ = return sTrue

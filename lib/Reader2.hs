module Reader2 where

import Control.Monad (forM_)
import Data.Foldable (foldrM)
import qualified Data.Functor
import Data.Map.Strict hiding (map)
import Data.Maybe (fromJust)
import Data.SBV
import Data.SBV.Internals (CV, genFromCV)
import Spec2
import Prelude hiding (lookup)

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
        >>= applyConstraints consts cellTypes

type PuzzleSolution = [[Word8]]

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
                    value <- lookup q d
                    Just $ genFromCV value
                )

        return $ map (map f) $ varNames ts
  return arrays

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

type SBoard = [[SWord8]]

-- | Creates an empty array of symbolic variables, where each{{{
-- variables takes values according to PuzzleStructure
emptyBoard :: PuzzleStructure -> Symbolic SBoard
emptyBoard ts = mapM2d (uncurry initialiseVar) (zip2d ts (varNames ts))
  where
    initialiseVar :: CellType -> String -> Symbolic SWord8
    initialiseVar t varName = do
      v <- free varName :: Symbolic SWord8
      constrain $ v .> 0 .&& v .< literal (noValues t + 1)
      return v

-- }}}

varNames :: PuzzleStructure -> [[String]]
varNames = zipWith f varNamesInf
  where
    f = zipWith const
    varNamesInf = [["X" ++ show i ++ show j | j <- [0 ..]] | i <- [0 ..]]

writeLiterals :: PuzzleState -> SBoard -> Symbolic SBoard -- {{{
writeLiterals s = mapM2d (uncurry f) . zip2d s
  where
    f :: Maybe Word8 -> SWord8 -> Symbolic SWord8
    f (Just n) x = constrain (x .== literal n) >> return x
    f Nothing x = return x

-- }}}

applyConstraints :: [Constraint] -> PuzzleStructure -> SBoard -> Symbolic SBool
applyConstraints cs cellTypes board = sAnd <$> mapM (\c -> applyConstraint c cellTypes board) cs
  where
    applyConstraint :: Constraint -> PuzzleStructure -> SBoard -> Symbolic SBool
    applyConstraint _ _ _ = return sTrue

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

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module PuzzleDSL.Internal.Solve
  ( asPredicate,
    solveAll,
    solve,
    printSols,
    PuzzleSolution,
  )
where

import Control.Monad (forM_)
import Data.Map.Strict hiding (filter, map)
import Data.Maybe (fromJust)
import Data.SBV hiding (solve)
import Data.SBV.Internals (genFromCV)
import PuzzleDSL.Internal.Rule (applyRules)
import PuzzleDSL.Internal.Spec
import PuzzleDSL.Internal.Utils hiding (lookup)
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
        Boolean -> do
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

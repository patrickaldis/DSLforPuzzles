module Tests.Features.Count where

import Data.Map.Strict
import Data.SBV (Word8)
import PuzzleDSL.DSL
import Test.Hspec
import Tests.Utils (shouldSolveTo)

countTests :: SpecWith ()
countTests = describe "Counting" $ do
  describe "Puzzle Tests" $ do
    it "Count Puzzle" $ do
      countPuzzleInstance `shouldSolveTo` [[1, 1, 0], [1, 0, 1], [0, 1, 1]]

countTotal = 2

countPuzzle :: PuzzleClass
countPuzzle =
  PuzzleClass
    { name = "Counting Puzzle",
      rules =
        [ ForAll
            numCell
            ( \x ->
                [ ForAll
                    numCell
                    ( \y ->
                        [ --
                          Constrain $
                            If
                              (Exp $ row x .== row y)
                              (Count [For numCell (\z -> Exp $ row x .== row z .&& bVal z)] (\result -> Exp $ result .== countTotal)),
                          Constrain $
                            If
                              (Exp $ col x .== col y)
                              (Count [For numCell (\z -> Exp $ col x .== col z .&& bVal z)] (\result -> Exp $ result .== countTotal))
                        ]
                    )
                ]
            )
        ],
      types = [numCell]
    }

numCell :: CellType
numCell =
  CellType
    { typeName = "Num",
      values = Boolean,
      propertySets = empty
    }

countPuzzleInstance :: PuzzleInstance
countPuzzleInstance =
  PuzzleInstance
    { puzzleclass = countPuzzle,
      structure = replicate 3 . replicate 3 $ numCell,
      state =
        let n = CellState {valueState = Nothing, propertyStates = empty}
         in replicate 3 . replicate 3 $ n
    }

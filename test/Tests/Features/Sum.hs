module Tests.Features.Sum (sumTests) where

import Data.Map.Strict (empty)
import Data.Maybe (fromJust)
import Data.SBV (Word8, unliteral)
import PuzzleDSL.DSL
import PuzzleDSL.Internal.Features.Sum (cellSum)
import Test.Hspec
import Tests.Utils (shouldSolveTo)

sumTests :: SpecWith ()
sumTests = describe "Summation" $ do
  describe "Module Functions" $ do
    it "cellSum" $ do
      cellSumTest `shouldBe` 9
  describe "Puzzle Tests" $ do
    it "Sum Puzzle" $ do
      sumPuzzleInstance `shouldSolveTo` [[5, 3, 1], [2, 3, 4], [2, 3, 4]]

cellSumTest :: Word8
cellSumTest =
  let input =
        [ [1, 1, 1],
          [1, 1, 1],
          [1, 1, 1]
        ]
      output = cellSum input
   in fromJust . unliteral $ output

sumPuzzle :: PuzzleClass
sumPuzzle =
  PuzzleClass
    { name = "Bool",
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
                              (Sum [For numCell (\z -> (If (Exp $ row x .== row z) (Exp $ nVal z)))] (\result -> Exp $ result .== 9)),
                          Constrain $
                            If
                              (Exp $ col x .== col y)
                              (Sum [For numCell (\z -> (If (Exp $ col x .== col z) (Exp $ nVal z)))] (\result -> Exp $ result .== 9))
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
      values = Numeric 9,
      propertySets = empty
    }

sumPuzzleInstance :: PuzzleInstance
sumPuzzleInstance =
  PuzzleInstance
    { puzzleclass = sumPuzzle,
      structure = replicate 3 . replicate 3 $ numCell,
      state =
        let n = CellState {valueState = Nothing, propertyStates = empty}
         in replicate 3 . replicate 3 $ n
    }

module Tests.Puzzles.Sudoku (sudokuTests) where

import Data.Map.Strict (empty)
import Data.Word (Word8)
import PuzzleDSL.DSL
import Test.Hspec
import Tests.Utils
import Prelude hiding (div)

sudokuTests :: SpecWith ()
sudokuTests =
  describe "Sudoku" $ do
    it "Easy" $ do
      easySudoku `shouldSolveTo` easySudokuSol

numberCell :: CellType
numberCell =
  CellType
    { typeName = "Number",
      values = Numeric 9,
      propertySets = empty
    }

sudoku :: PuzzleClass
sudoku =
  PuzzleClass
    { name = "Sudoku",
      rules =
        [ ForAll
            numberCell
            ( \x ->
                [ ForAll
                    numberCell
                    ( \y ->
                        [ --
                          Constrain $ Exp $ (row x .== row y) .=> (nVal x ./= nVal y),
                          Constrain $ Exp $ (col x .== col y) .=> (nVal x ./= nVal y),
                          Constrain $
                            Exp $
                              ((col x `div` 3 .== col y `div` 3) .&& (row x `div` 3 .== row y `div` 3))
                                .=> (nVal x ./= nVal y)
                        ]
                    )
                ]
            )
        ],
      types = [numberCell]
    }

easySudoku :: PuzzleInstance
easySudoku =
  PuzzleInstance
    { puzzleclass = sudoku,
      structure = replicate 9 . replicate 9 $ numberCell,
      state =
        let noMaybes =
              [ [1, 0, 3, 0, 0, 0, 0, 8, 0],
                [0, 0, 6, 0, 4, 8, 0, 0, 0],
                [0, 4, 0, 0, 0, 0, 0, 0, 0],
                [2, 0, 0, 0, 9, 6, 1, 0, 0],
                [0, 9, 0, 8, 0, 1, 0, 4, 0],
                [0, 0, 4, 3, 2, 0, 0, 0, 8],
                [0, 0, 0, 0, 0, 0, 0, 7, 0],
                [0, 0, 0, 1, 5, 0, 4, 0, 0],
                [0, 6, 0, 0, 0, 0, 2, 0, 3]
              ]
            f :: Word8 -> CellState
            f 0 = CellState {valueState = Nothing, propertyStates = empty}
            f x = CellState {valueState = Just (NumericEntry $ literal x), propertyStates = empty}
         in map (map f) noMaybes
    }

easySudokuSol :: [[Word8]]
easySudokuSol =
  [ [1, 7, 3, 2, 6, 9, 5, 8, 4],
    [9, 2, 6, 5, 4, 8, 3, 1, 7],
    [5, 4, 8, 7, 1, 3, 9, 2, 6],
    [2, 8, 7, 4, 9, 6, 1, 3, 5],
    [3, 9, 5, 8, 7, 1, 6, 4, 2],
    [6, 1, 4, 3, 2, 5, 7, 9, 8],
    [4, 5, 9, 6, 3, 2, 8, 7, 1],
    [8, 3, 2, 1, 5, 7, 4, 6, 9],
    [7, 6, 1, 9, 8, 4, 2, 5, 3]
  ]

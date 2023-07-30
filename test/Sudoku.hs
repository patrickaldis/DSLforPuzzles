module Sudoku (sudokuTests) where

import DSL
import Prelude hiding (div)
import Test.Hspec
import Data.Word (Word8)

sudokuTests :: SpecWith ()
sudokuTests = 
  describe "sudoku" $ do
    it "easy sudoku" $ do
      solveAll easySudoku >>= (\x -> head x `shouldBe` easySudokuSol)

sudoku :: Problem
sudoku =
  let numberCell =
        CellType
          { cellName = "Number",
            noValues = 9
          }
   in Problem
        { name = "Sudoku",
          structure = replicate 9 . replicate 9 $ numberCell,
          constraints =
            [ ForAll
                numberCell
                ( \x ->
                    [ ForAll
                        numberCell
                        ( \y ->
                            [ --
                              Exp $ (row x .== row y) .=> (value x ./= value y),
                              Exp $ (col x .== col y) .=> (value x ./= value y),
                              Exp $
                                ((col x `div` 3 .== col y `div` 3) .&& (row x `div` 3 .== row y `div` 3))
                                  .=> (value x ./= value y)
                            ]
                        )
                    ]
                )
            ]
        }

easySudoku :: PuzzleInstance
easySudoku =
  PuzzleInstance
    { problem = sudoku,
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
            f :: Word8 -> Maybe Word8
            f 0 = Nothing
            f x = Just x
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

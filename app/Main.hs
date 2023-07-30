{-# OPTIONS_GHC -Wall #-}

module Main where

import DSL
import Data.Word (Word8)
import Prelude hiding (div)

main :: IO ()
main = solveAll testInstance >>= printSols (Just testInstance)

testProblem :: Problem
testProblem =
  let numberCell =
        CellType
          { cellName = "Test Type 1",
            noValues = 9
          }
   in Problem
        { name = "Test Problem 1",
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

testInstance :: PuzzleInstance
testInstance =
  PuzzleInstance
    { problem = testProblem,
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

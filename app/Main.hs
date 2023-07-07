{-# OPTIONS_GHC -Wall #-}

module Main where

import Data.SBV hiding (name)
import Data.SBV.Internals ((.=>))
import Reader2
import Spec2

main :: IO ()
main = solveAll testInstance >>= printSols (Just testInstance)

testProblem :: Problem
testProblem =
  let t1 =
        CellType
          { cellName = "Test Type 1",
            noValues = 9
          }
   in Problem
        { name = "Test Problem 1",
          structure = replicate 9 . replicate 9 $ t1,
          constraints =
            [ ForAll
                t1
                ( \x ->
                    [ ForAll
                        t1
                        ( \y ->
                            [ --
                              Exp $ (col x .== col y) .&& (row x .== row y),
                              Exp $
                                ((row x .== row y) .=> (value x ./= value y))
                                  .&& ((col x .== col y) .=> (value x ./= value y))
                                  .&& ( ( (col x `sDiv` 3 .== col y `sDiv` 3)
                                            .&& (row x `sDiv` 3 .== row y `sDiv` 3)
                                        )
                                          .=> (value x ./= value y)
                                      )
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
              [ [0, 6, 0, 0, 0, 0, 0, 1, 0],
                [0, 0, 0, 6, 5, 1, 0, 0, 0],
                [1, 0, 7, 0, 0, 0, 6, 0, 2],
                [6, 2, 0, 3, 0, 5, 0, 9, 4],
                [0, 0, 3, 0, 0, 0, 2, 0, 0],
                [4, 8, 0, 9, 0, 7, 0, 3, 6],
                [9, 0, 6, 0, 0, 0, 4, 0, 8],
                [0, 0, 0, 7, 9, 4, 0, 0, 0],
                [0, 5, 0, 0, 0, 0, 0, 7, 0]
              ]
            f :: Word8 -> Maybe Word8
            f 0 = Nothing
            f x = Just x
         in map (map f) noMaybes
    }

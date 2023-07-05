{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad
import Control.Monad.Trans.List
import Data.List (intersperse)
import Data.SBV hiding (name)
import Reader2
import Spec2

main :: IO ()
main = solveAll testInstance >>= printSols (Just testInstance)

testProblem :: Problem
testProblem =
  Problem
    { name = "Test Problem 1",
      structure =
        let t1 =
              CellType
                { cellName = "Test Type 1",
                  noValues = 3
                }
         in [ [t1, t1, t1],
              [t1, t1, t1]
            ],
      constraints = []
    }

testInstance :: PuzzleInstance
testInstance =
  PuzzleInstance
    { problem = testProblem,
      state =
        [ [Nothing, Just 1, Just 3],
          [Just 2, Just 1, Just 1]
        ]
    }

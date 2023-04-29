{-# OPTIONS_GHC -Wall #-}

module Main where

import Control.Monad (foldM)
import Data.SBV
import Reader (convert)
import Spec

main :: IO ()
main = do
  let p = convert problem
  putStrLn "Single Solve:"
  r <- sat p
  print r
  putStrLn "All Solve"
  r' <- allSat p
  print "Solved"
  print r'

problem :: Constraint
problem =
  ForAll
    ( \x ->
        [ Exp $ x .> 0,
          Exp $ x .<= 9,
          ForAll
            ( \y ->
                [ Exp $ y .> 0,
                  Exp $ y .<= 9,
                  Exp $ x + y .== 7
                ]
            )
        ]
    )

problem2 :: Constraint
problem2 =
  ForAll
    ( \x ->
        [ Exp $ x .>= 0,
          Exp $ x .<= 4,
          ForAll
            ( \y ->
                [ Exp $ y .>= 0,
                  Exp $ y .<= 4,
                  Exp $ (y .^ (2 :: SInteger) - (x .^ (3 :: SInteger) + x + 1)) `sMod` 5 .== 0
                ]
            )
        ]
    )

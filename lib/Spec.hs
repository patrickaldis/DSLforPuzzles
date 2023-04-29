module Spec where

import Data.SBV

type Symbol = Int

data Constraint
  = ForAll (SBV Integer -> [Constraint])
  | Exp SBool

example :: Constraint
example =
  ForAll
    ( \x ->
        [ Exp $ x .> 1,
          Exp $ x .< 3
        ]
    )

example1 :: Constraint
example1 =
  ForAll
    ( \x ->
        [ ForAll
            ( \y ->
                [ 
                Exp $ x .< 11,
                  Exp $ x .> 4,
                  Exp $ x .> y,
                  Exp $ y .< 8,
                  Exp $ y .> 4
                ]
            )
        ]
    )

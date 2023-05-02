module Spec where

import Data.SBV

type Symbol = SInteger

data Constraint
  = ForAll (Symbol -> [Constraint])
  | Exp SBool

example2 :: Constraint
example2 =
  ForAll
    ( \x ->
        [ Exp $ x .> 1,
          Exp $ x .< 3
        ]
    )

example :: Constraint
example =
  ForAll
    ( \x ->
        [ Exp $ (x .> 4) .&& (x .< 11),
          ForAll
            ( \y ->
                [ Exp $ (y .> 4) .&& (y .< 8),
                  Exp $ x .> y
                ]
            )
        ]
    )

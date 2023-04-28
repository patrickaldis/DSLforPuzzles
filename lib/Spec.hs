module Spec where
import Data.SBV

type Symbol = Int

-- data SBool
--   = And SBool SBool
--   | Or SBool SBool
--   | Not SBool
--   | GT Symbol Symbol
--   | EQ Symbol Symbol
--   | Lit Bool
--
data Constraint
  = ForAll (SBV Integer -> [Constraint])
  | Exp SBool

example :: Constraint
example =
  ForAll
    ( \x ->
        [
          ForAll
            ( \y ->
                [
                  Exp $ x .> y,
                  Exp $ x .> 8,
                  Exp $ y .== 7
                ]
            )
        ]
    )

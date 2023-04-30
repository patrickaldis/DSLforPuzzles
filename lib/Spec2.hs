module Spec2 where

import Data.SBV

type Symbol = Int

data Constraint
  = ForAll (SBV Integer -> [Constraint])
  | Exp SBool

data Cell a = Cell
  { x :: Int,
    y :: Int,
    value :: a
  }

type Board a = [[Maybe (Cell a)]]

{-# OPTIONS_GHC -Wall #-}
module Main where
import Data.SBV
import Control.Monad (foldM)
import Spec

main :: IO ()
main = do
  r <- allSat problem
  print r

problem :: Predicate
problem = do x <- free "x" :: Symbolic SInteger
             y <- free "y" :: Symbolic SInteger
             constrain $ x .>= 0 :: Symbolic ()
             constrain $ y .>= 0 :: Symbolic ()
             return $ x + y .== 3 :: Symbolic SBool

-- convert :: Constraint -> Predicate
-- convert (ForAll f) = do
--   x <- free "a" :: Symbolic SInteger
--   let cs = f x :: [Constraint]
--       -- res :: SBV Bool
--   -- foldM :: ( b -> a -> m b)
--   --       :: b
--   --       :: t a   [Constraint]
--   --       :: m b
--   foldM (\s c -> s .&& convert c) (True :: SBV Bool) cs
--

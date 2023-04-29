module Reader (convert) where

import Data.SBV hiding (SBool)
import Spec

convert :: Constraint -> Predicate
convert = convert' ["X" ++ show n| n <- [1..]]

-- problem :: Predicate
-- problem = do x <- free "x" :: Symbolic SInteger
--              y <- free "y" :: Symbolic SInteger
--              constrain $ x .>= 0 :: Symbolic ()
--              constrain $ y .>= 0 :: Symbolic ()
--              return $ x + y .== 3 :: Symbolic SBool

convert' :: [String] -> Constraint -> Predicate
convert' (l:ls) (ForAll f) = do
  x <- free l :: Symbolic SInteger
  let cs = f x :: [Constraint]
  mapM_ (convert' ls) cs
  return sTrue
convert' _ (Exp e) = constrain e >> return sTrue
convert' _ _ = return sTrue


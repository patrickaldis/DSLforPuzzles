module Tests.Utils where

import PuzzleDSL.DSL
import Test.Hspec (shouldBe)

shouldSolveTo :: PuzzleInstance -> PuzzleSolution -> IO ()
shouldSolveTo prob sol = solve prob >>= (`shouldBe` Just sol)

isUnsolveable :: PuzzleInstance -> IO ()
isUnsolveable prob = solve prob >>= (`shouldBe` Nothing)

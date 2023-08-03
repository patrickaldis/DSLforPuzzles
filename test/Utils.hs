module Utils where
import Test.Hspec (shouldBe)
import DSL


shouldSolveTo :: PuzzleInstance -> PuzzleSolution -> IO ()
shouldSolveTo prob sol = solveAll prob >>= (\x -> head x `shouldBe` sol)

module Tests.Features.Connectivity (connectivityTests) where

import Data.Maybe (fromJust)
import Data.SBV hiding (name)
import qualified Data.SBV.List as L
import Data.SBV.Tuple
import PuzzleDSL.Internal.Features.Component
import Test.Hspec
import Prelude hiding (lookup)

connectivityTests :: SpecWith ()
connectivityTests = describe "Connectivity" $ do
  describe "Module Functions" $ do
    it "slookup (literals)" $ do
      shouldBe
        (fromJust . unliteral $ neighbors (4, 4) $ literal (1, 1))
        [(2, 1), (1, 2)]
    it "neighbors (literals)" $ do
      shouldBe
        (fromJust . unliteral $ slookup (literal (2, 2)) simple)
        True
    it "<~> (literals)" $ do
      conTest `shouldBe` True
    it "slookup (unknowns)" $ do
      allSat unknownLookup >>= (`shouldBe` "Solution #1:\n  s0 = (3,3) :: (Word8, Word8)\nSolution #2:\n  s0 = (4,3) :: (Word8, Word8)\nSolution #3:\n  s0 = (3,1) :: (Word8, Word8)\nSolution #4:\n  s0 = (1,1) :: (Word8, Word8)\nSolution #5:\n  s0 = (3,4) :: (Word8, Word8)\nSolution #6:\n  s0 = (4,4) :: (Word8, Word8)\nSolution #7:\n  s0 = (1,4) :: (Word8, Word8)\nSolution #8:\n  s0 = (1,2) :: (Word8, Word8)\nSolution #9:\n  s0 = (2,2) :: (Word8, Word8)\nSolution #10:\n  s0 = (2,1) :: (Word8, Word8)\nFound 10 different solutions.") . show
    it "neighbors (unknowns)" $ do
      allSat unknownNeighbors >>= (`shouldBe` "Solution #1:\n  s0 = (1,2) :: (Word8, Word8)\nThis is the only solution.") . show
    it "<~> (unknowns)" $ do
      conTestUnknown >>= \r -> r `shouldBe` []

simple :: [[SBool]]
simple =
  map2d
    literal
    [ [True, True, False, True],
      [True, True, False, False],
      [True, False, True, True],
      [False, False, True, True]
    ]

simpleSol :: [[Word8]]
simpleSol =
  [ [1, 1, 0, 2],
    [1, 1, 0, 0],
    [1, 0, 3, 3],
    [0, 0, 3, 3]
  ]

conTest :: Bool
conTest =
  fromJust . unliteral $
    isConnectedIx
      simple
      L.nil
      (literal (1, 1))
      (literal (1, 1))

conTestUnknown :: IO String
conTestUnknown = do
  r <-
    allSat
      ( do
          x <- free_
          let t = sTrue
              f = sFalse
              bs =
                [ [t, t, t, t],
                  [x, f, f, f],
                  [t, f, f, f],
                  [t, f, f, f]
                ]
              statement =
                isConnectedIx
                  bs
                  L.nil
                  (literal (3, 1))
                  (literal (1, 2))
          pure $ statement .== sTrue
      )
  return . show $ r

unknownLookup :: Symbolic SBool
unknownLookup = do
  x <- free_
  constrain $ isIndex x
  return $ slookup x simple .== sTrue

unknownNeighbors :: Symbolic SBool
unknownNeighbors = do
  x <- free_
  return $ neighbors (4, 4) (literal (2, 2)) .== L.implode [literal (3, 2), literal (2, 3), x, literal (2, 1)]

isIndex :: SBV (Word8, Word8) -> SBool
isIndex x = let (i, j) = untuple x in sAnd [1 .<= i, i .<= 4, 1 .<= j, j .<= 4]

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

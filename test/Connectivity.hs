module Connectivity (connectivityTests) where

import Component
import DSL
import Data.Map.Strict (empty)
import Data.Maybe (fromJust)
import Data.SBV hiding (name)
import qualified Data.Set as S
import Test.Hspec
import Utils
import Prelude hiding (lookup)

connectivityTests :: SpecWith ()
connectivityTests = describe "Connectivity" $ do
  describe "Module Functions" $ do
    it "neighbors (literals)" $ do
      shouldBe (neighbors (4, 4) (1, 1)) [(2, 1), (1, 2)]
    it "dfs (literals)" $ do
      shouldBe (unlitdfs $ dfs simple (1, 1) (S.fromList simpleIx) 1) unlitresult
    it "components (literals)" $ do
      shouldBe (unLit2d $ components simple) simpleSol
    it "neighbors (unknowns)" $ do
      True `shouldBe` False
    it "dfs (unknowns)" $ do
      True `shouldBe` False
    it "components (unknowns)" $ do
      allSat unknown >>= (`shouldBe` "") . show

-- it "Simple Connectivity" $ do
--   connectedToTest `shouldSolveTo` connectedToTestSol
unlitdfs :: ([Label], [(Word8, Word8)]) -> [(Word8, Word8, Word8)]
unlitdfs (ls, ix) = map (\((i, j), k) -> (i, j, fromJust $ unliteral k)) ls

unlitresult = [(1, 1, 1), (2, 1, 1), (3, 1, 1), (4, 1, 0), (3, 2, 0), (2, 2, 1), (2, 3, 0), (1, 2, 1), (1, 3, 0)]

simple :: [[SBool]]
simple =
  map2d
    literal
    [ [True, True, False, True],
      [True, True, False, False],
      [True, False, True, True],
      [False, False, True, True]
    ]

simpleIx :: [(Word8, Word8)]
simpleIx = [(toEnum i, toEnum j) | i <- [1 .. 4], j <- [1 .. 4]]

simpleSol :: [[Word8]]
simpleSol =
  [ [1, 1, 0, 2],
    [1, 1, 0, 0],
    [1, 0, 3, 3],
    [0, 0, 3, 3]
  ]

unknown :: Symbolic SBool
unknown = do
  x <- free_ :: Symbolic SBool
  let t = sTrue
      f = sFalse
      bs =
        [ [x, t, t, t],
          [t, f, f, f],
          [t, f, f, f],
          [t, f, f, f]
        ]
      conmap = components bs
      val c = lookup c conmap
  return (val (4, 1) .== val (1, 4))

unLit2d :: [[SWord8]] -> [[Word8]]
unLit2d = map2d (fromJust . unliteral)

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d f = map (map f)

-- connectedTo :: PuzzleClass
-- connectedTo =
--   PuzzleClass
--     { name = "Connected To Test",
--       rules =
--         [ CountComponents
--             [For boolCell (\c -> Exp $ bVal c .== sTrue)]
--             ( \comp ->
--                 ForAll
--                   boolCell
--                   ( \x ->
--                       [ ForAll
--                           boolCell
--                           ( \y ->
--                               [ --
--                                 Constrain $
--                                   ConnectedBy
--                                     x
--                                     y
--                                     comp
--                                     (\r -> Exp $ r)
--                               ]
--                           )
--                       ]
--                   )
--             )
--         ],
--       types = [boolCell]
--     }

-- connectedToTest :: PuzzleInstance
-- connectedToTest =
--   PuzzleInstance
--     { puzzleclass = connectedTo,
--       structure = replicate 4 . replicate 4 $ boolCell,
--       state =
--         let t = CellState {valueState = Just $ BoolEntry $ literal True, propertyStates = empty}
--             f = CellState {valueState = Just $ BoolEntry $ literal False, propertyStates = empty}
--             n = CellState {valueState = Nothing, propertyStates = empty}
--          in [ [t, n, f, f],
--               [f, n, n, f],
--               [f, f, n, n],
--               [f, f, f, t]
--             ]
--     }

-- connectedToTestSol :: PuzzleSolution
-- connectedToTestSol =
--   [ [1, 1, 0, 0],
--     [0, 1, 1, 0],
--     [0, 0, 1, 1],
--     [0, 0, 0, 1]
--   ]

-- boolCell :: CellType
-- boolCell =
--   CellType
--     { typeName = "Bool",
--       values = Bool,
--       propertySets = empty
--     }

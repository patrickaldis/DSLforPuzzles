module Connectivity (connectivityTests) where

import Data.Maybe (fromJust)
import Data.SBV hiding (name)
import qualified Data.SBV.List as L
import Data.SBV.Tuple
import PuzzleDSL.Internal.Component
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
    it "dfs (literals)" $ do
      shouldBe
        (fromJust . unliteral $ dfs' simple (literal (1, 1)) (literal simpleIx) 1)
        dfsSol
    -- it "applyDFS (literals)" $ do
    --   shouldBe (fromJust . unliteral $ applyDFS simple $ literal ([], simpleIx, 1)) ([], [], 0)
    -- it "components (literals)" $ do
    --   shouldBe (show $ components simple) ""
    it "slookup (unknowns)" $ do
      allSat unknownLookup >>= (`shouldBe` "Solution #1:\n  s0 = (3,3) :: (Word8, Word8)\nSolution #2:\n  s0 = (4,3) :: (Word8, Word8)\nSolution #3:\n  s0 = (3,1) :: (Word8, Word8)\nSolution #4:\n  s0 = (1,1) :: (Word8, Word8)\nSolution #5:\n  s0 = (3,4) :: (Word8, Word8)\nSolution #6:\n  s0 = (4,4) :: (Word8, Word8)\nSolution #7:\n  s0 = (1,4) :: (Word8, Word8)\nSolution #8:\n  s0 = (1,2) :: (Word8, Word8)\nSolution #9:\n  s0 = (2,2) :: (Word8, Word8)\nSolution #10:\n  s0 = (2,1) :: (Word8, Word8)\nFound 10 different solutions.") . show
    it "neighbors (unknowns)" $ do
      allSat unknownNeighbors >>= (`shouldBe` "Solution #1:\n  s0 = (1,2) :: (Word8, Word8)\nThis is the only solution.") . show
    it "dfs (unknowns)" $ do
      allSat unknownDFS >>= (`shouldBe` "Solution #1:\n  s0 = True :: Bool\nThis is the only solution.") . show
    -- it "components (unknowns)" $ do
    --   allSat unknownComp >>= (`shouldBe` "") . show

    it "components (unknowns)" $ do
      allSat unknownComp >>= (`shouldBe` "") . show

-- describe "Puzzle Tests" $ do
--   it "Simple Connectivity" $ do
--     connectedToTest `shouldSolveTo` connectedToTestSol

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

simpleIx :: [(Word8, Word8)]
simpleIx = [(toEnum i, toEnum j) | i <- [1 .. 4], j <- [1 .. 4]]

dfsSol :: ([((Word8, Word8), Word8)], [(Word8, Word8)])
dfsSol =
  ( [ ((1, 1), 1),
      ((1, 2), 1),
      ((1, 3), 0),
      ((2, 2), 1),
      ((2, 1), 1),
      ((3, 1), 1),
      ((3, 2), 0),
      ((4, 1), 0),
      ((2, 3), 0)
    ],
    [ (1, 1),
      (1, 2),
      (1, 3),
      (2, 2),
      (2, 1),
      (3, 1),
      (3, 2),
      (4, 1),
      (2, 3)
    ]
  )

unknownLookup :: Symbolic SBool
unknownLookup = do
  x <- free_
  constrain $ isIndex x
  return $ slookup x simple .== sTrue

unknownNeighbors :: Symbolic SBool
unknownNeighbors = do
  x <- free_
  return $ neighbors (4, 4) (literal (2, 2)) .== L.implode [literal (3, 2), literal (2, 3), x, literal (2, 1)]

unknownDFS :: Symbolic SBool
unknownDFS = do
  x <- free_
  let t = sTrue
      f = sFalse
      bs =
        [ [t, t, t, t],
          [x, f, f, f],
          [t, f, f, f],
          [t, f, f, f]
        ]
      ix = literal (1, 1)

      (ls, _) = untuple $ dfs' bs ix (literal simpleIx) 1
  return $ literal ((1, 3), 1) `L.elem` ls

unknownComp :: Symbolic SBool
unknownComp = do
  x <- free_
  -- y <- free_
  let t = sTrue
      f = sFalse
      bs =
        [ [t, t, t, t],
          [x, f, f, f],
          [t, f, f, f],
          [t, f, f, f]
        ]
      cmp1 = dfs bs (literal (3, 1))
      cmp2 = dfs bs (literal (1, 3))
      cmp1' = sFst $ dfs' bs (literal (1, 1)) (literal simpleIx) 1
      cmp2' = sFst $ dfs' bs (literal (1, 3)) (literal simpleIx) 1
  return $
    sAnd
      [ literal ((1, 3), 1) `L.elem` cmp1'
      -- literal (3, 1) `L.elem` cmp2
      ]

isIndex :: SBV (Word8, Word8) -> SBool
isIndex x = let (i, j) = untuple x in sAnd [1 .<= i, i .<= 4, 1 .<= j, j .<= 4]

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
--                                     (\r -> If (Exp $ bVal x .== sTrue .&& bVal y .== sTrue) (Exp r))
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
--         let t = CellState {valueState = Just $ BoolEntry $ literal True, propertyStates = M.empty}
--             f = CellState {valueState = Just $ BoolEntry $ literal False, propertyStates = M.empty}
--             n = CellState {valueState = Nothing, propertyStates = M.empty}
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
--       propertySets = M.empty
--     }

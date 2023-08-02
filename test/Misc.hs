module Misc (miscTests) where

import DSL
import Test.Hspec
import Prelude hiding (div)
import Data.Word (Word8)

miscTests :: SpecWith ()
miscTests =
  describe "Simple Puzzles" $ do
    it "Bool Test" $ do
      solveAll boolTest >>= (\x -> head x `shouldBe` boolTestSol)

boolCell :: CellType
boolCell =
  CellType
    { typeName = "Bool",
      possibleValues = Bool
    }

bool :: PuzzleClass
bool =
  PuzzleClass
    { name = "Bool",
      rules =
        [ ForAll
            boolCell
            ( \x ->
                [ ForAll
                    boolCell
                    ( \y ->
                        [ --
                          Constrain $ Exp $ (row x .== row y) .=> (bValue x ./= bValue y),
                          Constrain $ Exp $ (col x .== col y) .=> (bValue x ./= bValue y)
                        ]
                    )
                ]
            )
        ],
      types = [boolCell]
    }

boolTest :: PuzzleInstance
boolTest =
  PuzzleInstance
    { puzzleclass = bool,
      structure = replicate 2 . replicate 2 $ boolCell,
      state =
        let t = Just $ BoolEntry $ literal True
         in [[t, Nothing], [Nothing, t]]
    }

boolTestSol :: [[Word8]]
boolTestSol =
  [[1, 0], [0, 1]]

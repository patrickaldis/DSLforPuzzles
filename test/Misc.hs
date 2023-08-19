module Misc (miscTests) where

import Connectivity
import Data.Map.Strict
import Data.SBV (sTrue)
import Data.Word (Word8)
import PuzzleDSL.DSL
import Test.Hspec
import Utils
import Prelude hiding (div)

miscTests :: SpecWith ()
miscTests =
  describe "Language Features" $ do
    it "Bools" $ do
      boolTest `shouldSolveTo` boolTestSol
    it "Properties" $ do
      propTest `shouldSolveTo` propTestSol
    connectivityTests

boolCell :: CellType
boolCell =
  CellType
    { typeName = "Bool",
      values = Bool,
      propertySets = empty
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
                          Constrain $
                            If
                              (Exp $ row x .== row y)
                              (Exp $ bVal x ./= bVal y),
                          Constrain $
                            If
                              (Exp $ col x .== col y)
                              (Exp $ bVal x ./= bVal y)
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
        let t = CellState {valueState = Just $ BoolEntry $ literal True, propertyStates = empty}
            n = CellState {valueState = Nothing, propertyStates = empty}
         in [[t, n], [n, t]]
    }

boolTestSol :: [[Word8]]
boolTestSol =
  [[1, 0], [0, 1]]

propCell :: CellType
propCell =
  CellType
    { typeName = "Number",
      values = Numeric 8,
      propertySets =
        fromList
          [ ("Value", Numeric 40)
          ]
    }

prop :: PuzzleClass
prop =
  PuzzleClass
    { name = "Prop",
      rules =
        [ ForAll
            propCell
            ( \x ->
                [Constrain $ Exp $ nVal x .== nProp "Value" x]
            )
        ],
      types = [propCell]
    }

propTest :: PuzzleInstance
propTest =
  PuzzleInstance
    { puzzleclass = prop,
      structure = replicate 2 . replicate 2 $ propCell,
      state =
        let c x = CellState {valueState = Nothing, propertyStates = fromList [("Value", NumericProp x)]}
         in [[c 1, c 2], [c 3, c 4]]
    }

propTestSol :: PuzzleSolution
propTestSol = [[1, 2], [3, 4]]

module Tests.Features.Bool (boolTests) where

import Data.Map.Strict (empty)
import Data.SBV (Word8)
import PuzzleDSL.DSL
import Test.Hspec
import Tests.Utils

boolTests :: SpecWith ()
boolTests =
  it "Bools" $ do
    boolTest `shouldSolveTo` boolTestSol

boolCell :: CellType
boolCell =
  CellType
    { typeName = "Bool",
      values = Boolean,
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
         in [ [t, n],
              [n, t]
            ]
    }

boolTestSol :: [[Word8]]
boolTestSol =
  [ [1, 0],
    [0, 1]
  ]

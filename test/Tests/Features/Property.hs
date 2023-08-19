module Tests.Features.Property (propertyTests) where

import Data.Map.Strict (fromList)
import PuzzleDSL.DSL
import Test.Hspec
import Tests.Utils

propertyTests :: SpecWith ()
propertyTests =
  it "Properties" $ do
    propTest `shouldSolveTo` propTestSol

propCell :: CellType
propCell =
  CellType
    { typeName = "Number",
      values = Numeric 8,
      propertySets =
        fromList [("Value", Numeric 40)]
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

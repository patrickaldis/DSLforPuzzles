module Tests.Features.Spec (featureTests) where

import Test.Hspec
import Tests.Features.Bool (boolTests)
import Tests.Features.Connectivity (connectivityTests)
import Tests.Features.Count (countTests)
import Tests.Features.Property (propertyTests)
import Tests.Features.Sum (sumTests)

featureTests :: SpecWith ()
featureTests =
  describe "Language Features" $ do
    boolTests
    propertyTests
    sumTests
    countTests
    connectivityTests

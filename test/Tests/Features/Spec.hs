module Tests.Features.Spec (featureTests) where

import Test.Hspec
import Tests.Features.Bool (boolTests)
import Tests.Features.Connectivity (connectivityTests)
import Tests.Features.Property (propertyTests)

featureTests :: SpecWith ()
featureTests =
  describe "Language Features" $ do
    boolTests
    propertyTests
    connectivityTests

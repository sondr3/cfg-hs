module CFG.PythonParser (spec) where

import Test.Hspec

spec :: Spec
spec = parallel $ do
  it "does nothing" $ do
    True `shouldBe` True

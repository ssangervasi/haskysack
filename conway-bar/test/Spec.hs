import Test.Hspec

import qualified Conway
import qualified Data.Matrix

main :: IO ()
main = hspec $ do
  describe "Conway" $ do
    it "Bool matrix" $ do
      let falseGrid = Data.Matrix.matrix 3 3 (\_ -> False)
      Conway.gameOfLife 3 `shouldBe` falseGrid

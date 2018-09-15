module RFC2550Spec (spec) where

import Test.Hspec
import RFC2550

spec :: Spec
spec = do
  describe "Lists to timestamps" $ do
    it "[] ->  0"
      listToTimestamp [] `shouldBe` "0000"

    it "1000 12 31 13 45 16 8 -> 10001231134516008" $ do
      let inputList = [1000 12 31 13 45 16 8]
          expected = "10001231134516008"
      listToTimestamp inputList `shouldBe` expected
    
    it "12 1 5 1 -> 0012010501" $ do

  xdescribe "Datatype Style" $ do
    describe "RFC2550" $ do
      it "exists" $ do
        let
          constructed = RFC2550
            { year = 1992
            , month = 4
            , day = 29
            , hour = 4
            , minute = 20
            , second = 10
            , thousandths = [314, 596]
            }
        constructed `shouldSatisfy` not . (== epoch )
    
    describe "epoch" $ do
      it "constructs an RFC2550 with all zeroes" $ do
        let
          zeroes = RFC2550
            { year = 0
            , month = 0
            , day = 0
            , hour = 0
            , minute = 0
            , second = 0
            , thousandths = []
            }
        epoch `shouldBe` zeroes

    xdescribe "fromList" $ do
      it "empty list" $ do
        (fromList []) `shouldBe` epoch

      it "[year]" $ do
        let rfcFromList = fromList [2]
        rfcFromList `shouldBe` RFC2550 { year = 2 }

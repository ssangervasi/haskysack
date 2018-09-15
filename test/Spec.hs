import Test.Hspec
import Lib
import qualified RFC2550Spec

main :: IO ()
main = hspec $ do
  describe "RFC2550Spec" RFC2550Spec.spec

  describe "popAround" $ do
    it "moves the head to the end" $ do
      popAround [1..10] `shouldBe` ([2..10] ++ [1])

    it "just returns an empty list" $ do
      popAround [] `shouldBe` ([] :: [Char])
      -- [] `shouldBe` []

  describe "Horse" $ do
    let theHorse = Horse { name = "Barry", hooves = 4 }

    it "makes a horse" $ do
      name theHorse `shouldBe` "Barry"
      hooves theHorse `shouldBe` 4

    
    it "can edit the "
      let newHorse = 
      name theHorse `shouldBe` "Barry"
      hooves theHorse `shouldBe` 4

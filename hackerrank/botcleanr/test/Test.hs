import Test.Hspec
import Botcleanr

main :: IO ()
main = hspec $ do
  describe "botcleanr" $ do
    let testFieldString = "0 0\n"
                          ++ "b---d\n"
                          ++ "-----\n"
                          ++ "-----\n"
                          ++ "-----\n"
                          ++ "-----"

    it "reads field spec" $ do
      (show $ readString testFieldString)
        `shouldBe`
        "Field {width = 5, height = 5, botPos = (0,0), dirtyCells = [(4,0)]}"

    it "move towards dirty" $ do
      (whatToDo $ readString testFieldString) `shouldBe` Go RIGHT

    it "cleans" $ do
      let testFieldString = "0 4\n----d\n-----\n-----\n-----\n-----"
      (whatToDo $ readString testFieldString) `shouldBe` CLEAN

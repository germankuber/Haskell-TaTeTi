import Lib
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "playMark" $ do
        it "Return Nothing if play in the same position" $ 
            let markToAdd = Mark {markType = Cross, position = MarkPosition 1 1}
                mark1 = Mark Cross (MarkPosition 1 1)
                mark2 = Mark Cross (MarkPosition 1 2)
            in playMark markToAdd   [mark1, mark2] `shouldBe` Nothing
        it "Return List of Marks with Current Mark" $ 
            let markToAdd = Mark {markType = Cross, position = MarkPosition 1 3}
                mark1 = Mark Circle (MarkPosition 1 1)
                mark2 = Mark Cross (MarkPosition 1 2)
            in playMark markToAdd   [mark1, mark2] `shouldBe` Just [markToAdd,mark1, mark2]
        it "Return Nothing if try to play two time same mark" $ 
            let markToAdd = Mark {markType = Cross, position = MarkPosition 1 3}
                mark1 = Mark Cross (MarkPosition 1 1)
                mark2 = Mark Circle (MarkPosition 1 2)
            in playMark markToAdd   [mark1, mark2] `shouldBe` Nothing
    describe "createMarkPosition" $ do
        it "Return Nothing x > 3" $ 
            createMarkPosition (4,1) `shouldBe` Nothing
        it "Return Nothing x < 1" $ 
            createMarkPosition (0,1) `shouldBe` Nothing
        it "Return Nothing y > 3" $ 
            createMarkPosition (1,4) `shouldBe` Nothing
        it "Return Nothing y < 3" $ 
            createMarkPosition (1,4) `shouldBe` Nothing
    describe "createMark" $ do
        it "Return the Mark" $ 
            createMark Cross (MarkPosition 1 1) `shouldBe`  Mark { markType = Cross , position = MarkPosition 1 1}


import Test.Hspec

type Decimal = Int
type ShadokNumber = String

shadok :: Decimal -> ShadokNumber
shadok 0  = "◯"
shadok 1  = "–"
shadok 2  = "⌟"
shadok 3  = "⊿"
shadok x  = shadok ( x `div` 4 ) ++ shadok ( x `mod` 4 )


main = hspec $ do

    describe "Shadok numeric base" $ do
        it "0, 1, 2, 3 should be ◯ – ⌟ ⊿" $ do
             shadok 0 `shouldBe` "◯"
             shadok 1 `shouldBe` "–"
             shadok 2 `shouldBe` "⌟"
             shadok 3 `shouldBe` "⊿"


    describe "Multiple of four" $ do
        it "With only one ◯" $ do
            shadok 4  `shouldBe` "–◯"
            shadok 8  `shouldBe` "⌟◯"
            shadok 12 `shouldBe` "⊿◯"

        it "With multiple ◯" $ do
            shadok 16 `shouldBe` "–◯◯"

    describe "Any number which is not a multiple of four" $ do
        it "two shadoks verb number" $ do
            shadok 7  `shouldBe` "–⊿"
            shadok 15 `shouldBe` "⊿⊿"
            shadok 5  `shouldBe` "––"

        it "three shadoks verb number" $ do
            shadok 19 `shouldBe` "–◯⊿"

        it "big number" $ do
            shadok 9999 `shouldBe` "⌟–⊿◯◯⊿⊿"

        it "very big number" $ do 
            shadok 47850 `shouldBe` "⌟⊿⌟⌟⊿⌟⌟⌟"

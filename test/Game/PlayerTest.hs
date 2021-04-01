module Game.PlayerTest (tests) where

import Test.Hspec
import Game.Player (Card(..))

tests :: Spec
tests = do
  spec_card

spec_card :: Spec
spec_card = do
  describe "Card" $
    describe "show" $ do
      it "show Apollo returns \"Apollo\"" $ do
        show Apollo `shouldBe` "Apollo"

      it "show Artemis returns \"Artemis\"" $ do
        show Artemis `shouldBe` "Artemis"

      it "show Atlas returns \"Atlas\"" $ do
        show Atlas `shouldBe` "Atlas"

      it "show Demeter returns \"Demeter\"" $ do
        show Demeter `shouldBe` "Demeter"

      it "show Hephastus returns \"Hephastus\"" $ do
        show Hephastus `shouldBe` "Hephastus"

      it "show Minotaur returns \"Minotaur\"" $ do
        show Minotaur `shouldBe` "Minotaur"

      it "show Pan returns \"Pan\"" $ do
        show Pan `shouldBe` "Pan"

      it "show Prometheus returns \"Prometheus\"" $ do
        show Prometheus `shouldBe` "Prometheus"

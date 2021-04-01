{-# LANGUAGE OverloadedStrings #-}

module JSON.PlayerTest where

import qualified Data.Aeson as JSON

import Test.Hspec

import JSON.Player
import Game.Player

tests :: Spec
tests = do
  spec_player
  spec_pre_player
  spec_card

spec_player :: Spec
spec_player = do
  describe "Player" $ do
    describe "Player JSON" $ do
      it "{\"card\": \"Apollo\", \"tokens\":[[1,1],[2,2]]} decodes to Player Apollo [(1,1), (2,2)]" $ do
        JSON.eitherDecode "{\"card\": \"Apollo\", \"tokens\":[[1,1],[2,2]]}"
          `shouldBe`
          Right (Player Apollo [(1,1), (2,2)])

      it "Player Apollo [(1,1), (2,2)] encodes to {\"card\": \"Apollo\", \"tokens\":[[1,1],[2,2]]}" $ do
        JSON.encode (Player Apollo [(1,1), (2,2)])
        `shouldBe`
        "{\"card\":\"Apollo\",\"tokens\":[[1,1],[2,2]]}"

spec_pre_player :: Spec
spec_pre_player = do
  describe "PrePlayer" $ do
    describe "PrePlayer JSON decoding" $ do
      it "{\"card\":\"Apollo\"} decodes to PrePlayer Apollo" $ do
        JSON.eitherDecode "{\"card\":\"Apollo\"}" `shouldBe` Right (PrePlayer Apollo)

      it "{\"card\": \"Artemis\"} decodes to PrePlayer Artemis" $ do
        JSON.eitherDecode "{\"card\": \"Artemis\"}" `shouldBe` Right (PrePlayer Artemis)

      it "{\"card\": \"Atlas\"} decodes to PrePlayer Atlas" $ do
        JSON.eitherDecode "{\"card\": \"Atlas\"}" `shouldBe` Right (PrePlayer Atlas)

      it "{\"card\": \"Demeter\"} decodes to PrePlayer Demeter" $ do
        JSON.eitherDecode "{\"card\": \"Demeter\"}" `shouldBe` Right (PrePlayer Demeter)

      it "{\"card\": \"Hephastus\"} decodes to PrePlayer Hephastus" $ do
        JSON.eitherDecode "{\"card\": \"Hephastus\"}" `shouldBe` Right (PrePlayer Hephastus)

      it "{\"card\": \"Minotaur\"} decodes to PrePlayer Minotaur" $ do
        JSON.eitherDecode "{\"card\": \"Minotaur\"}" `shouldBe` Right (PrePlayer Minotaur)

      it "{\"card\": \"Pan\"} decodes to PrePlayer Pan" $ do
        JSON.eitherDecode "{\"card\": \"Pan\"}" `shouldBe` Right (PrePlayer Pan)

      it "{\"card\": \"Prometheus\"} decodes to PrePlayer Prometheus" $ do
        JSON.eitherDecode "{\"card\": \"Prometheus\"}" `shouldBe` Right (PrePlayer Prometheus)

    describe "PrePlayer JSON Encoding" $ do
      it "PrePlayer Apollo encodes to {\"card\":\"Apollo\"}"  $ do
        JSON.encode (PrePlayer Apollo) `shouldBe` "{\"card\":\"Apollo\"}"

      it "PrePlayer Artemis encodes to {\"card\": \"Artemis\"}"  $ do
        JSON.encode (PrePlayer Artemis) `shouldBe` "{\"card\":\"Artemis\"}"

      it "PrePlayer Atlas encodes to {\"card\": \"Atlas\"}"  $ do
        JSON.encode (PrePlayer Atlas) `shouldBe` "{\"card\":\"Atlas\"}"

      it "PrePlayer Demeter encodes to {\"card\": \"Demeter\"}"  $ do
        JSON.encode (PrePlayer Demeter) `shouldBe` "{\"card\":\"Demeter\"}"

      it "PrePlayer Hephastus encodes to {\"card\": \"Hephastus\"}"  $ do
        JSON.encode (PrePlayer Hephastus) `shouldBe` "{\"card\":\"Hephastus\"}"

      it "PrePlayer Minotaur encodes to {\"card\": \"Minotaur\"}"  $ do
        JSON.encode (PrePlayer Minotaur) `shouldBe` "{\"card\":\"Minotaur\"}"

      it "PrePlayer Pan encodes to {\"card\": \"Pan\"}"  $ do
        JSON.encode (PrePlayer Pan) `shouldBe` "{\"card\":\"Pan\"}"

      it "PrePlayer Prometheus encodes to {\"card\": \"Prometheus\"}"  $ do
        JSON.encode (PrePlayer Prometheus) `shouldBe` "{\"card\":\"Prometheus\"}"

spec_card :: Spec
spec_card = do
  describe "Card" $ do
    describe "Card JSON decoding" $ do
      it "\"Apollo\" decodes to Apollo" $ do
        JSON.eitherDecode "\"Apollo\"" `shouldBe` Right Apollo

      it "\"Artemis\" decodes to Artemis" $ do
        JSON.eitherDecode "\"Artemis\"" `shouldBe` Right Artemis

      it "\"Atlas\" decodes to Atlas" $ do
        JSON.eitherDecode "\"Atlas\"" `shouldBe` Right Atlas

      it "\"Demeter\" decodes to Demeter" $ do
        JSON.eitherDecode "\"Demeter\"" `shouldBe` Right Demeter

      it "\"Hephastus\" decodes to Hephastus" $ do
        JSON.eitherDecode "\"Hephastus\"" `shouldBe` Right Hephastus

      it "\"Minotaur\" decodes to Minotaur" $ do
        JSON.eitherDecode "\"Minotaur\"" `shouldBe` Right Minotaur

      it "\"Pan\" decodes to Pan" $ do
        JSON.eitherDecode "\"Pan\"" `shouldBe` Right Pan

      it "\"Prometheus\" decodes to Prometheus" $ do
        JSON.eitherDecode "\"Prometheus\"" `shouldBe` Right Prometheus

      it "\"foo\" fails to decode as a Card" $ do
        (JSON.decode "\"foo\"" :: Maybe Card) `shouldBe` Nothing

      it "[1,2,3] fails to decode as a Card" $ do
        (JSON.decode "[1,2,3]" :: Maybe Card) `shouldBe` Nothing

    describe "Card JSON encoding" $ do
      it "Apollo encodes to \"Apollo\"" $ do
        JSON.encode Apollo `shouldBe` "\"Apollo\""

      it "Artemis encodes to \"Artemis\"" $ do
        JSON.encode Artemis `shouldBe` "\"Artemis\""

      it "Atlas encodes to \"Atlas\"" $ do
        JSON.encode Atlas `shouldBe` "\"Atlas\""

      it "Demeter encodes to \"Demeter\"" $ do
        JSON.encode Demeter `shouldBe` "\"Demeter\""

      it "Hephastus encodes to \"Hephastus\"" $ do
        JSON.encode Hephastus `shouldBe` "\"Hephastus\""

      it "Minotaur encodes to \"Minotaur\"" $ do
        JSON.encode Minotaur `shouldBe` "\"Minotaur\""

      it "Pan encodes to \"Pan\"" $ do
        JSON.encode Pan `shouldBe` "\"Pan\""

      it "Prometheus encodes to \"Prometheus\"" $ do
        JSON.encode Prometheus `shouldBe` "\"Prometheus\""

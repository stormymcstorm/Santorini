{-# LANGUAGE OverloadedStrings #-}
module JSON.StateTest where

import qualified Data.Aeson as JSON
import Test.Hspec
import Control.Exception
import JSON.State
import Game.State
import Game.Player

tests :: Spec
tests = do
  describe "State JSON" $ do
    describe "JSON decoding State" $ do
      it "should decode NonePlaced" $ do
        JSON.eitherDecode "[{\"card\": \"Apollo\"}, {\"card\": \"Artemis\"}]"
        `shouldBe`
        Right (NonePlaced (
          PrePlayer Apollo, PrePlayer Artemis
          ))

      it "should decode OnePlaced" $ do
        JSON.eitherDecode "[{\"card\": \"Artemis\"}, {\"card\": \"Apollo\", \"tokens\":[[1,1],[2,2]]}]"
        `shouldBe`
        Right (OnePlaced (PrePlayer Artemis, Player Apollo [(1,1), (2,2)]))

      it "should decode BothPlaced" $ do
        JSON.eitherDecode "[{\"card\": \"Artemis\", \"tokens\":[[3,3],[4,4]]}, {\"card\": \"Apollo\", \"tokens\":[[1,1],[2,2]]}]"
        `shouldBe`
        Right (BothPlaced (Player Artemis [(3,3), (4,4)], Player Apollo [(1,1), (2,2)]))

      let self = Player Apollo [(2,4), (1,4)]
      let other = Player Artemis [(3,5), (2,4)]

      it "should decode InProgress" $ do
        JSON.eitherDecode "{\"turn\":5,\"players\":[{\"card\":\"Apollo\",\"tokens\":[[2,4],[1,4]]},{\"card\":\"Artemis\",\"tokens\":[[3,5],[2,4]]}],\"spaces\":[[1,1,4,4,0],[4,4,2,2,1],[4,4,4,3,2],[2,1,0,2,0],[1,1,1,0,0]]}"
        `shouldBe`
        Right (InProgress (mkBoard 5 (self, other) [1,1,4,4,0,4,4,2,2,1,4,4,4,3,2,2,1,0,2,0,1,1,1,0,0]))

      it "should fail on non board or players" $ do
        JSON.decode "1" `shouldBe` (Nothing :: Maybe State)

    describe "JSON encoding State" $ do
      it "should fail to encode NonePlaced" $ do
        evaluate (JSON.encode (NonePlaced (PrePlayer Apollo, PrePlayer Artemis)))
        `shouldThrow`
        anyException

      it "should encode OnePlaced" $ do
        JSON.encode (OnePlaced (PrePlayer Artemis, Player Apollo [(1,1), (2,2)]))
        `shouldBe`
        "[{\"card\":\"Artemis\"},{\"card\":\"Apollo\",\"tokens\":[[1,1],[2,2]]}]"

      it "should encode BothPlaced" $ do
        JSON.encode (BothPlaced (Player Artemis [(3,3), (4,4)], Player Apollo [(1,1), (2,2)]))
        `shouldBe`
        "[{\"card\":\"Artemis\",\"tokens\":[[3,3],[4,4]]},{\"card\":\"Apollo\",\"tokens\":[[1,1],[2,2]]}]"

      let self = Player Apollo [(2,4), (1,4)]
      let other = Player Artemis [(3,5), (2,4)]

      it "should encode InProgress" $ do
        let board = mkBoard 5 (self, other) [1,1,4,4,0,4,4,2,2,1,4,4,4,3,2,2,1,0,2,0,1,1,1,0,0]
        let string = "{\"turn\":5,\"players\":[{\"card\":\"Apollo\",\"tokens\":[[2,4],[1,4]]},{\"card\":\"Artemis\",\"tokens\":[[3,5],[2,4]]}],\"spaces\":[[1,1,4,4,0],[4,4,2,2,1],[4,4,4,3,2],[2,1,0,2,0],[1,1,1,0,0]]}"

        JSON.encode (InProgress board) `shouldBe` string

      it "should fail to encode Won" $ do
        let board = mkBoard 5 (self, other) [1,1,4,4,0,4,4,2,2,1,4,4,4,3,2,2,1,0,2,0,1,1,1,0,0]

        evaluate (JSON.encode (Won board))
        `shouldThrow`
        anyException

      it "should encode Lost" $ do
        let board = mkBoard 5 (self, other) [1,1,4,4,0,4,4,2,2,1,4,4,4,3,2,2,1,0,2,0,1,1,1,0,0]
        let string = "{\"turn\":5,\"players\":[{\"card\":\"Apollo\",\"tokens\":[[2,4],[1,4]]},{\"card\":\"Artemis\",\"tokens\":[[3,5],[2,4]]}],\"spaces\":[[1,1,4,4,0],[4,4,2,2,1],[4,4,4,3,2],[2,1,0,2,0],[1,1,1,0,0]]}"

        JSON.encode (Lost board) `shouldBe` string

module Main where

import Test.Hspec

import qualified Game.PlayerTest
import qualified Game.SpacesTest
import qualified Game.StateTest
import qualified JSON.PlayerTest
import qualified JSON.StateTest
-- import qualified Logic.StateTest

main :: IO ()
main = hspec $ do
  Game.PlayerTest.tests
  Game.SpacesTest.tests
  Game.StateTest.tests
  JSON.PlayerTest.tests
  JSON.StateTest.tests
  -- Logic.StateTest.tests

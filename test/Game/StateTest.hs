module Game.StateTest (tests) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Game.Player
import Game.Spaces
import Game.State

tests :: Spec
tests = do
  describe "Board" $ do
    let self = Player Apollo [(1,1), (2,2)]
    let other = Player Artemis [(3,3), (4,4)]

    describe "mkBoard" $ do
      it "should return build with 0 turn" $ do
        let board = mkInitialBoard (self, other)

        turn board `shouldBe` 0

      it "should return board with all 0 levels" $ do
        let board = mkInitialBoard (self, other)
        let spaces' = spaces board

        toLevelList spaces' `shouldBe` replicate 5 (replicate 5 0)

      it "should return spaces with player occupied spaces" $ do
        let board = mkInitialBoard (self, other)

        spaceAt (spaces board) (1,1) `shouldBe` Self 0
        spaceAt (spaces board) (2,2) `shouldBe` Self 0
        spaceAt (spaces board) (3,3) `shouldBe` Other 0
        spaceAt (spaces board) (4,4) `shouldBe` Other 0

    describe "swapPlayers" $ do
      it "should swap players in board" $ do
        let board = mkInitialBoard (self, other)
        let board' = swapPlayers board
        let players' = players board'

        players' `shouldBe` (toPlayerI other, toPlayerI self)

      it "should increment turn in board" $ do
        let board = mkInitialBoard (self, other)
        let board' = swapPlayers board

        turn board' `shouldBe` turn board + 1

  describe "Turn" $ do
    describe "doTurn" $ do
      let self = Player Apollo [(1,1), (2,2)]
      let other = Player Artemis [(3,3), (4,4)]

      it "should increment turn counter" $ do
        let board = mkInitialBoard (self, other)
        let turn' = Turn ((1,1), (1,2)) Nothing (Just [((1,1), 1)])
        let (_, board') = doTurn board turn'

        turn board' `shouldBe` turn board + 1

      it "should return board with swapped players" $ do
        let board = mkInitialBoard (self, other)
        let turn' = Turn ((1,1), (1,2)) Nothing (Just [((1,1), 1)])
        let (_, board') = doTurn board turn'

        players board' `shouldBe` (
            toPlayerI (Player Artemis [(3,3), (4,4)])
          , toPlayerI (Player Apollo [(1,2), (2,2)])
          )

      it "should return board with swapped players spaces" $ do
        let self = Player Artemis [(2,1), (2,2)]
        let other = Player Apollo [(5,1), (5,3)]

        let board = mkBoard 0 (self, other) [3,1,0,0,0, 0,0,0,0,0, 0,3,0,0,0, 1,0,0,0,0, 1,1,1,0,0]
        let turn' = Turn ((2,1), (1,3)) Nothing (Just [((1,2), 2)])
        let (_, board') = doTurn board turn'

        toSpaceList (spaces board') `shouldBe` [
              [Unoccupied 3, Unoccupied 2, Other 0,      Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Other 0,      Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 3, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 1, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Self 1,       Unoccupied 1, Self 1,       Unoccupied 0, Unoccupied 0]
          ]

      it "should return board with spaces reflecting move" $ do
        let board = mkInitialBoard (self, other)
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (_, board') = doTurn board turn'

        toSpaceList (spaces board') `shouldBe` [
              [Unoccupied 0, Other 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Other 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Self 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Self 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
          ]

      it "should return board with spaces reflecting build" $ do
        let board = mkInitialBoard (self, other)
        let turn' = Turn ((1,1), (1,2)) Nothing (Just [((1,1), 1)])
        let (_, board') = doTurn board turn'

        toSpaceList (spaces board') `shouldBe` [
              [Unoccupied 1, Other 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Other 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Self 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Self 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
          ]

      it "should return board with spaces reflecting displace" $ do
        let board = mkInitialBoard (self, other)
        let turn' = Turn ((2,2), (3,3)) (Just ((3,3), (2,2))) Nothing
        let (_, board') = doTurn board turn'

        toSpaceList (spaces board') `shouldBe` [
              [Other 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Self 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Other 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Self 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
          ]

        let board = mkInitialBoard (self, other)
        let turn' = Turn ((2,2), (3,3)) (Just ((3,3), (3,4))) (Just [((3,2), 1)])
        let (_, board') = doTurn board turn'

        toSpaceList (spaces board') `shouldBe` [
              [Other 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 1, Other 0, Self 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Self 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
          ]

      it "should return (True, _) when moving to level 3" $ do
        let board = mkBoard 0 (self, other) $ [2, 3, 0, 0, 0] ++ replicate 20 0
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (is_win, _) = doTurn board turn'

        is_win `shouldBe` True

      it "should return (False, _) when moving laterally to level 3" $ do
        let board = mkBoard 0 (self, other)
              $ [3, 3, 0, 0, 0] ++ replicate 20 0
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (is_win, _) = doTurn board turn'

        is_win `shouldBe` False

      it "should return (False, _) when moving to any non level 3" $ do
        let board = mkBoard 0 (self, other)
              $ [1, 2, 0, 0, 0] ++ replicate 20 0
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (is_win, _) = doTurn board turn'

        is_win `shouldBe` False

        let board = mkBoard 0 (self, other)
              $ [0, 1, 0, 0, 0] ++ replicate 20 0
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (is_win, _) = doTurn board turn'

        is_win `shouldBe` False

      it "should return (True, _) when moving down 3 levels for Pan" $ do
        let (Player _ tokens) = self
        let board = mkBoard 0 (Player Pan tokens, other)
              $ [3, 0, 0, 0, 0] ++ replicate 20 0
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (is_win, _) = doTurn board turn'

        is_win `shouldBe` True

      it "should return (True, _) when moving down 2 levels for Pan" $ do
        let (Player _ tokens) = self
        let board = mkBoard 0 (Player Pan tokens, other)
              $ [2, 0, 0, 0, 0] ++ replicate 20 0
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (is_win, _) = doTurn board turn'

        is_win `shouldBe` True

      it "should return (False, _) when moving down 1 level for Pan" $ do
        let (Player _ tokens) = self
        let board = mkBoard 0 (Player Pan tokens, other)
              $ [1, 0, 0, 0, 0] ++ replicate 20 0
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (is_win, _) = doTurn board turn'

        is_win `shouldBe` False

      it "should return (False, _) when moving laterally level for Pan" $ do
        let (Player _ tokens) = self
        let board = mkBoard 0 (Player Pan tokens, other)
              $ [1, 1, 0, 0, 0] ++ replicate 20 0
        let turn' = Turn ((1,1), (1,2)) Nothing Nothing
        let (is_win, _) = doTurn board turn'

        is_win `shouldBe` False




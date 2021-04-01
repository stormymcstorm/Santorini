module Game.SpacesTest (tests)  where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Ix (range)
import Data.Array (listArray, array)

import Game.Player
import Game.Spaces
import Game.State

tests :: Spec
tests = do
  describe "Space" $ do
    describe "unwrapSpace" $ do
      it "should return level for Unoccupied" $ do
        unwrapSpace (Unoccupied 3) `shouldBe` 3

      it "should return level for Self" $ do
        unwrapSpace (Self 3) `shouldBe` 3

      it "should return level for Other" $ do
        unwrapSpace (Other 3) `shouldBe` 3

    describe "spaceToLevel" $ do
      it "should return level for Unoccupied" $ do
        spaceToLevel (Unoccupied 3) `shouldBe` 3

      it "should return level for Self" $ do
        spaceToLevel (Self 3) `shouldBe` 3

      it "should return level for Other" $ do
        spaceToLevel (Other 3) `shouldBe` 3


    describe "isOccupied" $ do
      it "should return False for Unoccupied" $ do
        isOccupied (Unoccupied 3) `shouldBe` False

      it "should return True for Self" $ do
        isOccupied (Self 3) `shouldBe` True

      it "should return True for Other" $ do
        isOccupied (Other 3) `shouldBe` True

    describe "isUnoccupied" $ do
      it "should return True for Unoccupied" $ do
        isUnoccupied (Unoccupied 3) `shouldBe` True

      it "should return False for Self" $ do
        isUnoccupied (Self 3) `shouldBe` False

      it "should return False for Other" $ do
        isUnoccupied (Other 3) `shouldBe` False

    describe "setLevel" $ do
      prop "should replace level in Unoccupied" $
        \l -> setLevel (Unoccupied 1) l `shouldBe` Unoccupied l

      prop "should replace level in Self" $
        \l -> setLevel (Self 1) l `shouldBe` Self l

      prop "should replace level in Other" $
        \l -> setLevel (Other 1) l `shouldBe` Other l

  describe "Pos" $ do
    describe "posRange" $ do
      it "should return ((1,1),(5,5))" $ do
        posRange `shouldBe` ((1,1),(5,5))

    describe "boardPositions" $ do
      it "should return [(1,1)..(5,5)]" $ do
        boardPositions `shouldBe` range ((1,1),(5,5))

  describe "Spaces" $ do
    let p1 = Player Apollo [(1,1), (2,2)]
    let p1' = toPlayerI (Player Apollo [(1,1), (2,2)])
    let p2 = Player Artemis [(3,3), (4,4)]
    let p2' = toPlayerI (Player Artemis [(3,3), (4,4)])
    let ls = replicate 25 0

    describe "unwrapSpaces" $ do
      it "should return underlying array" $ do
       let arr = listArray posRange (replicate 25 (Unoccupied 0))

       unwrapSpaces (Spaces arr) `shouldBe` arr

    describe "mkSpaces" $ do
      it "should return spaces with correct spaces" $ do
        let arr = array posRange . map (\pos -> if isPlayerAt p1' pos then
              (pos, Self 0)
            else if isPlayerAt p2' pos then
              (pos, Other 0)
            else
              (pos, Unoccupied 0)) $ boardPositions

        unwrapSpaces (fst (mkSpaces (p1, p2) ls)) `shouldBe` arr

    describe "levelAt" $ do
      it "should return level at position" $ do
        let (spaces, _) = mkSpaces (p1, p2) ls

        levelAt spaces (1,1) `shouldBe` 0
        levelAt spaces (5,5) `shouldBe` 0
        levelAt spaces (2,4) `shouldBe` 0

    describe "spaceAt" $ do
      it "should return the space at the position" $ do
        let (spaces, _) = mkSpaces (p1, p2) ls

        spaceAt spaces (1,1) `shouldBe` Self 0
        spaceAt spaces (2,2) `shouldBe` Self 0
        spaceAt spaces (3,3) `shouldBe` Other 0
        spaceAt spaces (4,4) `shouldBe` Other 0
        spaceAt spaces (5,5) `shouldBe` Unoccupied 0
        spaceAt spaces (1,4) `shouldBe` Unoccupied 0

    describe "toLevelList" $ do
      it "should return 2d list of levels" $ do
        let (spaces, _) = mkSpaces (p1, p2) ls

        toLevelList spaces `shouldBe` replicate 5 (replicate 5 0)

    describe "toSpaceList" $ do
      it "should return a 2d list of spaces" $ do
        let (spaces, _) = mkSpaces (p1, p2) ls

        toSpaceList spaces `shouldBe` [
              [Self 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Self 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Other 0, Unoccupied 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Other 0, Unoccupied 0]
            , [Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0, Unoccupied 0]
          ]

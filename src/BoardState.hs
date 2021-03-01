{-# LANGUAGE OverloadedStrings #-}

module BoardState where

import Data.Maybe ( fromJust )
import qualified Data.Vector as Vec
import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON, ToJSON, toJSON, toEncoding, parseJSON, parseJSONList, decode)
import Board
import GameTree

-------------------------------------------------------------------------------
-- BoardState
-------------------------------------------------------------------------------

onePlacedExample :: BoardState
onePlacedExample = fromJust $ decode "[[[1,5],[4,2]]]"

data BoardState = NonePlaced
  | OnePlaced Player 
  | TwoPlaced Player Player
  | InProgress Board
  | Won Board
  | Lost Board
  deriving (Show)

instance FromJSON BoardState where 
  parseJSON v @ (JSON.Array a)
    | Vec.length a == 0 = return NonePlaced
    | Vec.length a == 1 = fmap (OnePlaced . head) . parseJSONList $ v
    | otherwise = mempty -- Should never have to decode a TwoPlaced
  parseJSON v = fmap InProgress . parseJSON $ v

instance ToJSON BoardState where
  toJSON NonePlaced = toJSON ([] :: [Player])
  toJSON (OnePlaced p) = toJSON [p]
  toJSON (TwoPlaced p1 p2) = toJSON [p1, p2]
  toJSON (InProgress b) = toJSON b
  toJSON (Won b) = toJSON b
  toJSON (Lost b) = toJSON b

  toEncoding NonePlaced = toEncoding ([] :: [Player])
  toEncoding (OnePlaced p) = toEncoding [p]
  toEncoding (TwoPlaced p1 p2) = toEncoding [p1, p2]
  toEncoding (InProgress b) = toEncoding b
  toEncoding (Won b) = toEncoding b
  toEncoding (Lost b) = toEncoding b

-------------------------------------------------------------------------------
-- Game Logic
-------------------------------------------------------------------------------

takeTurn :: BoardState -> BoardState
takeTurn = minmax 4 

firstPlacement :: Player
firstPlacement = [(1,1), (2,2)]

instance GameTree BoardState where
  isFinal (Won _) = True
  isFinal (Lost _) = True
  isFinal _ = False

  value (Won _) = maxBound 
  value (Lost _) = minBound 
  value _ = 0

  next (Won _) = [] 
  next (Lost _) = [] 

  next NonePlaced = return (OnePlaced firstPlacement)

  next (OnePlaced p) =
    let
      avaliable :: [Pos]
      avaliable = filter (`notElem` p) positions
    in do 
      tok1 <- avaliable
      tok2 <- filter (tok1 /=) avaliable 

      return (TwoPlaced p [tok1, tok2])

  next (TwoPlaced p1 p2) = next . InProgress . mkBoard $ (p1, p2) 

  next (InProgress b) = 
    case possibleTurns b of
      [] -> return (Lost b)
      ts -> do
        turn <- ts

        case doTurn b turn of
          (b',None) -> return (InProgress b')
          (b',Win) -> return (Won b')
  
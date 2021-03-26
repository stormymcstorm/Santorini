{-# LANGUAGE OverloadedStrings #-}
module Game.State where

import Data.Maybe (fromJust)
import Data.Aeson (
  (.:), (.=)
  , FromJSON(..)
  , ToJSON(..)
  , withObject
  )
import Board ( Board, Player, PrePlayer )
import Data.Foldable (asum)

import qualified Board
import qualified Data.Aeson as JSON

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

nonePlacedEx :: GameState
nonePlacedEx = fromJust $ JSON.decode "[{\"card\": \"Apollo\"}, {\"card\": \"Atlas\"}]"

onePlacedEx :: GameState
onePlacedEx = fromJust $ JSON.decode "[{\"card\": \"Apollo\"}, {\"card\": \"Atlas\", \"tokens\": [[1,1],[1,2]]}]"

bothPlacedEx :: GameState
bothPlacedEx = fromJust $ JSON.decode "[{\"card\": \"Apollo\", \"tokens\": [[2,2],[3,3]]}, {\"card\": \"Atlas\", \"tokens\": [[1,1],[1,2]]}]"

-------------------------------------------------------------------------------
-- GameState
-------------------------------------------------------------------------------

data GameState = 
    NonePlaced (PrePlayer, PrePlayer)
  | OnePlaced (PrePlayer, Player)
  | BothPlaced (Player, Player)
  | InProgress Board
  | Won Board
  | Lost Board

instance FromJSON GameState where 
  parseJSON v @ (JSON.Array _) = asum [
        BothPlaced <$> parseJSON v
      , OnePlaced <$> parseJSON v
      , NonePlaced <$> parseJSON v
    ]
  
  parseJSON v @ (JSON.Object _) = InProgress <$> parseJSON v

  parseJSON _ = fail "Expected an array of players or a game board"

instance ToJSON GameState where
  toJSON (OnePlaced players)  = toJSON players 
  toJSON (BothPlaced players) = toJSON players 
  toJSON (InProgress board)   = toJSON board
  toJSON (Won board)          = toJSON board
  toJSON (Lost board)         = toJSON board

  toEncoding (OnePlaced players)  = toEncoding players
  toEncoding (BothPlaced players) = toEncoding players
  toEncoding (InProgress board)   = toEncoding board
  toEncoding (Won board)          = toEncoding board
  toEncoding (Lost board)         = toEncoding board
    
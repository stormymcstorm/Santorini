{-# LANGUAGE OverloadedStrings #-}
module JSON.State where

import Data.Aeson (
  (.:), (.=)
  , FromJSON(..)
  , ToJSON(..)
  , withObject
  , withText
  )
import Data.Foldable (asum)
import Data.Aeson.Types (Parser)
import qualified Data.Text as T
import qualified Data.Aeson.Encoding as JSON.Encoding
import qualified Data.Aeson as JSON


import Game.State
import Game.Spaces

import JSON.Player

-------------------------------------------------------------------------------
-- State
-------------------------------------------------------------------------------

instance FromJSON State where
  parseJSON v @ (JSON.Array _) = asum [
        BothPlaced <$> parseJSON v
      , OnePlaced <$> parseJSON v
      , NonePlaced <$> parseJSON v
    ]

  parseJSON v @ (JSON.Object _) = InProgress <$> parseJSON v

  parseJSON _ = fail "Expected an array of players or a game board"

instance ToJSON State where
  toJSON (NonePlaced _)       = error "should not be writing out a noneplaced"
  toJSON (OnePlaced players)  = toJSON players
  toJSON (BothPlaced players) = toJSON players
  toJSON (InProgress board)   = toJSON board
  toJSON (Lost board)         = toJSON board
  toJSON (Won board)          = error "should not be writing out a lost game"

  toEncoding (NonePlaced _)       = error "should not be writing out a noneplaced"
  toEncoding (OnePlaced players)  = toEncoding players
  toEncoding (BothPlaced players) = toEncoding players
  toEncoding (InProgress board)   = toEncoding board
  toEncoding (Lost board)         = toEncoding board
  toEncoding (Won board)          = error "should not be writing out a lost game"

-------------------------------------------------------------------------------
-- Board
-------------------------------------------------------------------------------

instance FromJSON Board where
  parseJSON = withObject "board" $ \obj -> do
    turn <- obj .: "turn"
    players <- obj .: "players"
    spaces <- concat <$> (obj .: "spaces" :: Parser [[Level]])

    return $ mkBoard turn players spaces

instance ToJSON Board where
  toJSON (Board turn (self, other) spaces) = JSON.object [
        "turn"    .= turn
      , "players" .= (toPlayer self, toPlayer other)
      , "spaces"  .= toLevelList spaces
    ]

  toEncoding (Board turn (self, other) spaces) = JSON.pairs (
          "turn"    .= turn
      <>  "players" .= (toPlayer self, toPlayer other)
      <>  "spaces"  .= toLevelList spaces
    )

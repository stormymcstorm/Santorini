{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module JSON.Player where

import Data.Aeson (
  (.:), (.=)
  , FromJSON(..)
  , ToJSON(..)
  , withObject
  , withText
  )

import qualified Data.Text as T
import qualified Data.Aeson.Encoding as JSON.Encoding
import qualified Data.Aeson as JSON

import Game.Player (Card(..), PrePlayer(..), Player(..))

-------------------------------------------------------------------------------
-- Player
-------------------------------------------------------------------------------

instance FromJSON Player where
  parseJSON = withObject "player" $ \obj -> Player
    <$> obj .: "card"
    <*> obj .: "tokens"

instance ToJSON Player where
  toJSON (Player card tokens) = JSON.object [
      "card" .= card
    , "tokens" .= tokens
    ]

  toEncoding (Player card tokens) = JSON.pairs (
        "card" .= card
      <> "tokens" .= tokens
    )

-------------------------------------------------------------------------------
-- PrePlayer
-------------------------------------------------------------------------------

instance FromJSON PrePlayer where
  parseJSON = withObject "pre-player" $ \obj -> PrePlayer
    <$> obj .: "card"

instance ToJSON PrePlayer where
  toJSON (PrePlayer card) = JSON.object ["card" .= card]

  toEncoding (PrePlayer card) = JSON.pairs ("card" .= card)


-------------------------------------------------------------------------------
-- Card
-------------------------------------------------------------------------------

instance FromJSON Card where
  parseJSON = withText "Card" $ \case
    "Apollo"      -> pure Apollo
    "Artemis"     -> pure Artemis
    "Atlas"       -> pure Atlas
    "Demeter"     -> pure Demeter
    "Hephastus"   -> pure Hephastus
    "Minotaur"    -> pure Minotaur
    "Pan"         -> pure Pan
    "Prometheus"  -> pure Prometheus
    unknown       -> fail $ "Unknown card: " <> T.unpack unknown

instance ToJSON Card where
  toJSON Apollo     = JSON.String "Apollo"
  toJSON Artemis    = JSON.String "Artemis"
  toJSON Atlas      = JSON.String "Atlas"
  toJSON Demeter    = JSON.String "Demeter"
  toJSON Hephastus  = JSON.String "Hephastus"
  toJSON Minotaur   = JSON.String "Minotaur"
  toJSON Pan        = JSON.String "Pan"
  toJSON Prometheus = JSON.String "Prometheus"

  toEncoding Apollo     = JSON.Encoding.string "Apollo"
  toEncoding Artemis    = JSON.Encoding.string "Artemis"
  toEncoding Atlas      = JSON.Encoding.string "Atlas"
  toEncoding Demeter    = JSON.Encoding.string "Demeter"
  toEncoding Hephastus  = JSON.Encoding.string "Hephastus"
  toEncoding Minotaur   = JSON.Encoding.string "Minotaur"
  toEncoding Pan        = JSON.Encoding.string "Pan"
  toEncoding Prometheus = JSON.Encoding.string "Prometheus"

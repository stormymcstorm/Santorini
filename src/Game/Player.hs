module Game.Player (
      Card(..)
    , PrePlayer(..)
    , Player(..)
    -- , isPlayerAt
    , Token
  ) where

import Data.List ( intercalate )


-------------------------------------------------------------------------------
-- Player
-------------------------------------------------------------------------------

-- | A player that has placed their tokens
data Player = Player !Card ![Token]
  deriving (Eq)

-- -- | Returns true if and only if the player has a token at the given position.
-- isPlayerAt :: Player    -- ^ The player whose tokens to check
--           -> (Int, Int) -- ^ The position to check for
--           -> Bool       -- ^ Whether the player has a token at the given position
-- isPlayerAt = flip elem . tokens

instance Show Player where
  show (Player card tokens) =
    show card
    ++ "@"
    ++ (intercalate " & " . map show $ tokens)

-------------------------------------------------------------------------------
-- Token
-------------------------------------------------------------------------------

-- | The location of a player token
type Token = (Int, Int)

-------------------------------------------------------------------------------
-- PrePlayer
-------------------------------------------------------------------------------

-- | A player that has not placed their tokens yet
newtype PrePlayer = PrePlayer Card
  deriving (Eq)

instance Show PrePlayer where
  show (PrePlayer card) = show card

-------------------------------------------------------------------------------
-- Card
-------------------------------------------------------------------------------

-- | The cards that a player can be playing with
data Card =
    Apollo
  | Artemis
  | Atlas
  | Demeter
  | Hephastus
  | Minotaur
  | Pan
  | Prometheus
  deriving (Eq)

instance Show Card where
  show Apollo     = "Apollo"
  show Artemis    = "Artemis"
  show Atlas      = "Atlas"
  show Demeter    = "Demeter"
  show Hephastus  = "Hephastus"
  show Minotaur   = "Minotaur"
  show Pan        = "Pan"
  show Prometheus = "Prometheus"

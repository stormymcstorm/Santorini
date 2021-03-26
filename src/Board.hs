{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Board where

import Control.Monad ((<=<))
import Data.List ( groupBy )
import Data.Ix (range)
import Data.Array (Array, array, (!), (//))
import Data.Aeson (
  (.:), (.=)
  , FromJSON(..)
  , ToJSON(..)
  , withObject
  , withText
  )
import Data.Aeson.Types (Parser)
import Data.Bifunctor(Bifunctor(..))

import qualified Data.Array as Array
import qualified Data.Text as T
import qualified Data.Aeson.Encoding as JSON.Encoding
import qualified Data.Aeson as JSON
import qualified Data.Bifunctor as Bifunctor

import Debug.Trace (trace)


-------------------------------------------------------------------------------
-- Turn
-------------------------------------------------------------------------------

-- | A turn taken by a player
data Turn = Turn (Maybe Move) (Maybe Move) (Maybe Build)

type Move = (Pos, Pos)
type Build = [(Pos,Int)]

doTurn :: Board -> Turn -> (Bool, Board)
doTurn (Board turnN (self, other) spaces) (Turn move displace build) =
  let
    players' = (
        case displace of
          Nothing -> other
          Just (from, to) -> other {tokens = replaceIf (==from) to $ tokens other}
      , case move of
          Nothing -> self
          Just (from, to) -> self {tokens = replaceIf (==from) to $ tokens self}
      )

    mkMoveDelta :: Player -> [Pos] -> [(Pos, Pos)]
    mkMoveDelta player = filter (uncurry (==)) . zip (tokens player)

    self_moves_d = maybe [] return move
    other_moves_d = maybe [] return displace

    movesToSpaces :: [(Pos, Pos)] -> (Bool, [(Pos, Space)])
    movesToSpaces =
        fork (any isWin) (>>= toSpaces)
        . map (Bifunctor.bimap withLevel withLevel)
      where
        withLevel :: Pos -> (Pos, Int)
        withLevel = fork id (levelAt spaces)

        toSpaces :: ((Pos, Int), (Pos, Int)) -> [(Pos, Space)]
        toSpaces (from, to) = [
            Bifunctor.second Unoccupied from
          , Bifunctor.second Occupied to
          ]

        isWin :: ((Pos, Int), (Pos, Int)) -> Bool
        isWin ((_, from_l), (_, to_l)) = case card self of
          Pan -> (to_l == 3 &&  from_l /= 3) || (from_l - to_l >= 2)
          _   -> to_l == 3 &&  from_l /= 3

    (won, self_spaces) = movesToSpaces self_moves_d

    (_, other_spaces) = movesToSpaces other_moves_d

    build_spaces = case build of
      Nothing -> []
      Just builds ->
        if won then
          []
        else
          map (Bifunctor.second Unoccupied) builds

    spaces' = Spaces . (// concat [
        self_spaces
      , other_spaces
      , build_spaces
      ]) . unwrapSpaces $ spaces

  in (won, Board (turnN + 1) players' spaces')

replaceIf :: Eq a =>
          (a -> Bool) -- the predicate the determines if the element should be replaced
          -> a        -- the element to replace with
          -> [a]      -- the elements
          -> [a]      -- the elements with matching elements replaced
replaceIf pred with = map (\e -> if pred e then with else e)


-------------------------------------------------------------------------------
-- Board
-------------------------------------------------------------------------------

data Board = Board {
      getTurn :: !Int
    , getPlayers :: !(Player, Player)
    , getSpaces :: !Spaces
  }

-- | Creates an empty board with the given players
mkBoard :: (Player, Player) -> Board
mkBoard players = Board 0 players (mkSpaces players (replicate 25 0))

instance FromJSON Board where
  parseJSON = withObject "board" $ \obj -> do
    turn <- obj .: "turn"
    players <- obj .: "players"
    spaces <- mkSpaces players . concat <$> (obj .: "spaces" :: Parser [[Int]])

    return (Board turn players spaces)

instance ToJSON Board where
  toJSON (Board turn players spaces) = JSON.object [
        "turn"    .= turn
      , "players" .= players
      , "spaces"  .= toLevelsList spaces
    ]

  toEncoding (Board turn players spaces) = JSON.pairs (
          "turn"    .= turn
      <>  "players" .= players
      <>  "spaces"  .= toLevelsList spaces
    )

-------------------------------------------------------------------------------
-- Spaces
-------------------------------------------------------------------------------

-- | A 2d array representing the spaces on the board
newtype Spaces = Spaces {unwrapSpaces :: Array Pos Space}

-- | Creates a `Spaces` from the player positions and level array
mkSpaces :: (Player, Player) -> [Int] -> Spaces
mkSpaces (self, other) = Spaces
  . array posRange
  . zipWith tagSpace boardPositions
  where
    tagSpace :: Pos -> Int -> (Pos, Space)
    tagSpace pos level
      | isPlayerAt self pos || isPlayerAt other pos = (pos, Occupied level)
      | otherwise                                   = (pos, Unoccupied level)

-- | Gets the level at the given position
levelAt :: Spaces -> Pos -> Int
levelAt (Spaces arr) = unwrapSpace . (arr !)
-- levelAt (Spaces arr) = unwrapSpace . (arr !)

-- | Gets the neighbors of a space
neighbors :: Spaces -> Pos -> [(Pos, Space)]
neighbors (Spaces arr) p = filter (isNeighborOf p . fst) (Array.assocs  arr)
  where
    isNeighborOf :: Pos -> Pos -> Bool
    isNeighborOf (x,y) (x',y') =
      (x,y) /= (x',y')
      && abs (x' - x) <= 1
      && abs (y' - y) <= 1

-- | Converts the `Spaces` into a 2d list of levels
toLevelsList :: Spaces -> [[Int]]
toLevelsList = map (map (unwrapSpace . snd))
  . groupBy sameRow
  . Array.assocs
  . unwrapSpaces

-- | Converts the `Spaces` into a 2d list of spaces
toSpaceList :: Spaces -> [[Space]]
toSpaceList = map (map snd)
  . groupBy sameRow
  . Array.assocs
  . unwrapSpaces

sameRow :: (Pos, a) -> (Pos, a) -> Bool
sameRow ((r, _), _) ((r', _), _) = r == r'

instance Show Spaces where
  show = show . toLevelsList

-- | A space on the board which is either occupied or unoccupied
data Space = Unoccupied Int | Occupied Int

unwrapSpace :: Space -> Int
unwrapSpace (Unoccupied level) = level
unwrapSpace (Occupied level)   = level

-- | True if and only if the space is unoccupied
isUnoccupied :: Space -> Bool
isUnoccupied (Unoccupied _) = True
isUnoccupied _              = False

-- | A position on the board
type Pos = (Int, Int)

-- | The range of positions on the board
posRange :: (Pos, Pos)
posRange = ((1,1),(5,5))

-- | The positions on the board
boardPositions :: [Pos]
boardPositions = range posRange


-------------------------------------------------------------------------------
-- PrePlayer
-------------------------------------------------------------------------------

-- | A player that has not placed their tokens yet
newtype PrePlayer = PrePlayer Card
  deriving (Show)

instance FromJSON PrePlayer where
  parseJSON = withObject "pre-player" $ \obj -> PrePlayer
    <$> obj .: "card"

instance ToJSON PrePlayer where
  toJSON (PrePlayer card) = JSON.object ["card" .= card]

  toEncoding (PrePlayer card) = JSON.pairs ("card" .= card)

-------------------------------------------------------------------------------
-- Player
-------------------------------------------------------------------------------

-- | A player in the game each with a card and token positions
data Player = Player {
    card :: !Card
  , tokens :: ![Token]
  }
  deriving (Show)

-- | True iff one of the players tokens is at the position
isPlayerAt :: Player -> Pos -> Bool
isPlayerAt =  flip elem . tokens


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

-- | A player token
type Token = Pos

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

instance Show Card where
  show Apollo     = "Apollo"
  show Artemis    = "Artemis"
  show Atlas      = "Atlas"
  show Demeter    = "Demeter"
  show Hephastus  = "Hephastus"
  show Minotaur   = "Minotaur"
  show Pan        = "Pan"
  show Prometheus = "Prometheus"

instance FromJSON Card where
  parseJSON = withText "" $ \case
    "Apollo"      -> pure Apollo
    "Artemis"     -> pure Artemis
    "Atlas"       -> pure Atlas
    "Demeter"     -> pure Demeter
    "Hephastus"   -> pure Hephastus
    "Minotaur"    -> pure Minotaur
    "Pan"         -> pure Pan
    "Prometheus"  -> pure Prometheus
    unknown       -> fail $ "Unknown card:" <> T.unpack unknown

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

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

fork :: (a -> b) -> (a -> c) -> a -> (b, c)
fork f g a = (f a, g a)

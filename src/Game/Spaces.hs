module Game.Spaces (
    Spaces(..)
    , unwrapSpaces
    -- , mkSpaces
    , toLevelList
    , toSpaceList
    , spaceAt
    , levelAt
    , mapSpaces
    , updateSpaces
    , Space(..)
    , unwrapSpace
    , spaceToLevel
    , isOccupied
    , isUnoccupied
    , setLevel
    , toSelf
    , toOther
    , toUnoccupied
    , Level(..)
    , Pos(..)
    , neighbors
    , posRange
    , boardPositions
  ) where

import Data.List (groupBy)
import Data.Ix (range)
import Data.Array (Array, array, (!), (//))
import qualified Data.Array as Array

-- import Game.Player (Player, isPlayerAt)

-------------------------------------------------------------------------------
-- Spaces
-------------------------------------------------------------------------------

-- | The board's spaces
newtype Spaces = Spaces (Array Pos Space)
  deriving (Eq)

-- | Returns the array that the spaces are backed by
unwrapSpaces :: Spaces -> Array Pos Space
unwrapSpaces (Spaces arr) = arr

-- | Returns the level at the given position
levelAt :: Spaces -> Pos -> Level
levelAt spaces = unwrapSpace . spaceAt spaces

-- | Returns the space at the given position
spaceAt :: Spaces -> Pos -> Space
spaceAt (Spaces arr) = (arr !)

-- | Converts the spaces to a 2d list of levels in row major order
toLevelList :: Spaces -> [[Level]]
toLevelList = map (map spaceToLevel) . toSpaceList

-- | Converts the spaces to a 2d list of spaces in row major order
toSpaceList :: Spaces -> [[Space]]
toSpaceList =
  map (map snd)
  . groupBy sameRow
  . Array.assocs
  . unwrapSpaces

sameRow :: (Pos, a) -> (Pos, a) -> Bool
sameRow ((r, _), _) ((r', _), _) = r == r'

neighbors :: Spaces -> Pos -> [(Pos, Space)]
neighbors (Spaces arr) = map (\p -> (p, arr ! p)) . neighboringPts

neighboringPts :: Pos -> [Pos]
neighboringPts (1,1) = [(1,2), (2,1), (2,2)]
neighboringPts (1,2) = [(1,1), (1,3), (2,1), (2,2), (2,3)]
neighboringPts (1,3) = [(1,2), (1,4), (2,2), (2,3), (2,4)]
neighboringPts (1,4) = [(1,3), (1,5), (2,3), (2,4), (2,5)]
neighboringPts (1,5) = [(1,4), (2,4), (2,5)]
neighboringPts (2,1) = [(1,1), (1,2), (2,2), (3,1), (3,2)]
neighboringPts (2,2) = [(1,1), (1,2), (1,3), (2,1), (2,3), (3,1), (3,2), (3,3)]
neighboringPts (2,3) = [(1,2), (1,3), (1,4), (2,2), (2,4), (3,2), (3,3), (3,4)]
neighboringPts (2,4) = [(1,3), (1,4), (1,5), (2,3), (2,5), (3,3), (3,4), (3,5)]
neighboringPts (2,5) = [(1,4), (1,5), (2,4), (3,4), (3,5)]
neighboringPts (3,1) = [(2,1), (2,2), (3,2), (4,1), (4,2)]
neighboringPts (3,2) = [(2,1), (2,2), (2,3), (3,1), (3,3), (4,1), (4,2), (4,3)]
neighboringPts (3,3) = [(2,2), (2,3), (2,4), (3,2), (3,4), (4,2), (4,3), (4,4)]
neighboringPts (3,4) = [(2,3), (2,4), (2,5), (3,3), (3,5), (4,3), (4,4), (4,5)]
neighboringPts (3,5) = [(2,4), (2,5), (3,4), (4,4), (4,5)]
neighboringPts (4,1) = [(3,1), (3,2), (4,2), (5,1), (5,2)]
neighboringPts (4,2) = [(3,1), (3,2), (3,3), (4,1), (4,3), (5,1), (5,2), (5,3)]
neighboringPts (4,3) = [(3,2), (3,3), (3,4), (4,2), (4,4), (5,2), (5,3), (5,4)]
neighboringPts (4,4) = [(3,3), (3,4), (3,5), (4,3), (4,5), (5,3), (5,4), (5,5)]
neighboringPts (4,5) = [(3,4), (3,5), (4,4), (5,4), (5,5)]
neighboringPts (5,1) = [(4,1), (4,2), (5,2)]
neighboringPts (5,2) = [(4,1), (4,2), (4,3), (5,1), (5,3)]
neighboringPts (5,3) = [(4,2), (4,3), (4,4), (5,2), (5,4)]
neighboringPts (5,4) = [(4,3), (4,4), (4,5), (5,3), (5,5)]
neighboringPts (5,5) = [(4,4), (4,5), (5,4)]

mapSpaces :: ((Pos, Space) -> (Pos, Space)) -> Spaces -> Spaces
mapSpaces f = Spaces . array posRange . map f . Array.assocs . unwrapSpaces

updateSpaces :: [(Pos, Space)] -> Spaces -> Spaces
updateSpaces ups = Spaces . (// ups) . unwrapSpaces

instance Show Spaces where
  show = unlines
    . map (concatMap show)
    . toSpaceList

-------------------------------------------------------------------------------
-- Space
-------------------------------------------------------------------------------

data Space =
    Unoccupied !Level
  | Self !Level
  | Other !Level
  deriving (Eq)

-- | Returns the level at the space
unwrapSpace :: Space -> Level
unwrapSpace (Unoccupied l) = l
unwrapSpace (Self l)       = l
unwrapSpace (Other l)      = l

-- | Returns the level at the space
spaceToLevel :: Space -> Level
spaceToLevel = unwrapSpace

-- | Returns True if and only if the space is occupied
isOccupied :: Space -> Bool
isOccupied = not . isUnoccupied

-- | Returns True if and only if the space is unoccupied
isUnoccupied :: Space -> Bool
isUnoccupied (Unoccupied _) = True
isUnoccupied _              = False

setLevel :: Space -> Level -> Space
setLevel (Unoccupied _) = Unoccupied
setLevel (Self _)       = Self
setLevel (Other _)      = Other

toSelf :: Space -> Space
toSelf (Unoccupied l) = Self l
toSelf (Self l)       = Self l
toSelf (Other l)      = Self l

toOther :: Space -> Space
toOther (Unoccupied l) = Other l
toOther (Self l)       = Other l
toOther (Other l)      = Other l

toUnoccupied :: Space -> Space
toUnoccupied (Unoccupied l) = Unoccupied l
toUnoccupied (Self l)       = Unoccupied l
toUnoccupied (Other l)      = Unoccupied l


instance Show Space where
  show (Unoccupied l) = " " ++ show l ++ " "
  show (Self l)       = "(" ++ show l ++ ")"
  show (Other l)      = "[" ++ show l ++ "]"

-------------------------------------------------------------------------------
-- Level
-------------------------------------------------------------------------------

-- | A space's level, i.e how tall the building on it is
type Level = Int

-------------------------------------------------------------------------------
-- Pos
-------------------------------------------------------------------------------

-- | A position on the board in row major order
type Pos = (Int, Int)

-- | The range of possible board positions
posRange :: (Pos, Pos)
posRange = ((1,1), (5,5))

-- | A list containing all the possible board positions
boardPositions :: [Pos]
boardPositions = range posRange

-- | Returns a list of neighboring positions
-- neighbors :: Pos -> [Pos]
-- neighbors (x,y) = filter isNeighbor boardPositions
--   where
--     isNeighbor (x', y') =
--       (x,y) /= (x', y')
--       && abs(x - x') <= 1
--       && abs(y - y') <= 1

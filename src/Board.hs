{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Board where 

import Control.Monad ( MonadPlus(mzero), forM, mapM ) 
import Data.Maybe (fromJust)
import Data.List ( groupBy, intercalate, elem ) 
import Data.Array (Array, Ix, array, range, (!), (//))
import qualified Data.Array as Array
import qualified Data.Vector as Vec
import qualified Data.Aeson as JSON
import Data.Aeson (
    (.:), (.=)
  , ToJSON
  , toJSON
  , toEncoding
  , FromJSON
  , encode
  , decode
  , withObject
  , withArray
  , parseJSON
  , parseJSONList
  )
import Data.Aeson.Types (Parser)

exampleBoard :: Board
exampleBoard = fromJust $ decode 
  "{\"players\":[[[2,3],[4,4]],[[2,5],[3,5]]],\"spaces\":[[0,0,0,0,2],[1,1,2,0,0],[1,0,0,3,0],[0,0,3,0,0],[0,0,0,1,4]],\"turn\":18}"

-- exampleEncodedBoard :: ByteString
exampleEncodedBoard = encode exampleBoard

-------------------------------------------------------------------------------
-- Turn
-------------------------------------------------------------------------------

data Turn = Turn Move (Maybe Pos) -- (to,from) build

instance Show Turn where 
  show (Turn move build) = show move ++ " + " ++ show build

-------------------------------------------------------------------------------
-- Move
-------------------------------------------------------------------------------

newtype Move = Move (Pos, Pos)

untagMove :: Move -> (Pos, Pos)
untagMove (Move m) = m

getTo :: Move -> Pos
getTo = snd . untagMove

getFrom :: Move -> Pos
getFrom = fst . untagMove

instance Show Move where
  show (Move (from, to)) = show from ++ " -> " ++ show to

-------------------------------------------------------------------------------
-- Board
-------------------------------------------------------------------------------

data Board = Board {
    getTurn :: !Int
  , getPlayers :: !Players
  , getSpaces :: !Spaces
  } 

data TurnResult = None | Win

mkBoard :: Players -> Board
mkBoard players = Board 0 players (mkSpaces players (replicate 25 0))


doTurn :: Board  -- ^ The board to perform the turn on
        -> Turn  -- ^ The turn to perform
        -> (Board, TurnResult) -- ^ The updated board 
doTurn (Board turn (self, other) spaces) (Turn (Move(from, to)) (Just build)) = 
  let
    players' = (other, map (\tok -> if tok == from then to else tok) self)

    from_l = levelAt spaces from
    to_l = levelAt spaces to
    build_l = levelAt spaces build

    spaces' = Spaces (untagSpaces spaces // [
        (from, Unoccupied from_l)
      , (to, Occupied to_l)
      , (build, Unoccupied (build_l + 1))
      ])
  in (Board (turn + 1) players' spaces', None)

doTurn (Board turn (self, other) spaces) (Turn (Move(from, to)) Nothing) = 
  let
    players' = (other, map (\tok -> if tok == from then to else tok) self)

    from_l = levelAt spaces from
    to_l = levelAt spaces to

    spaces' = Spaces (untagSpaces spaces // [
        (from, Unoccupied from_l)
      , (to, Occupied to_l)
      ])
  in (Board (turn + 1) players' spaces', Win)

possibleTurns :: Board -> [Turn]
possibleTurns b @ (Board _ _ spaces) = do
  move @ (Move (_, to)) <- possibleMoves b

  if levelAt spaces to == 3 then
    return (Turn move Nothing)
  else do
    (build, _) <- filter (canBuild . snd) . neighbors spaces $ to

    return (Turn move (Just build))
  where
    canBuild :: Space -> Bool
    canBuild (Unoccupied l) = l < 4
    canBuild _ = False 
  

possibleMoves :: Board -> [Move]
possibleMoves (Board _ (self, _) spaces) = do
  from <- self
  
  let level = levelAt spaces from

  (to, _) <- filter (isPossibleFrom level . snd) . neighbors spaces $ from

  return (Move (from, to))
  where
    isPossibleFrom :: Int -> Space -> Bool
    isPossibleFrom _ (Occupied _) = False
    isPossibleFrom l (Unoccupied l') = l' < 4 && l' <= l + 1

-- Some pretty printing for the Board (mainly for debugging)
instance Show Board where 
  show (Board turn (self, other) spaces ) = 
    "Turn = " ++ show turn ++ "\n" ++ boardStr spaces
    where
      boardStr = intercalate "\n\n"
        . map(unwords . map spaceStr)
        . groupBy sameRow 
        . Array.assocs
        . untagSpaces

      spaceStr :: (Pos, Space) -> [Char]
      spaceStr (_, Unoccupied l) = " " ++ show l ++ " "
      spaceStr (p, Occupied l) 
        | isPlayerAt self p = "(" ++ show l ++ ")" 
        | isPlayerAt other p =  "[" ++ show l ++ "]" 
      
instance FromJSON Board where
  parseJSON = withObject "Board" $ \o -> do
    turn <- o .: "turn"
    players <- o .: "players"
    spaces <- fmap (mkSpaces players . concat) (o .: "spaces" :: Parser [[Int]])

    return (Board turn players spaces)

instance ToJSON Board where 
  toJSON (Board turn  players spaces) = JSON.object [
      "turn" .= turn
    , "players" .= players
    , "spaces" .= toLevels spaces
    ]

  toEncoding (Board turn players spaces) = JSON.pairs (
        "turn" .= turn
    <>  "players" .= players
    <>  "spaces" .= toLevels spaces
    )

-------------------------------------------------------------------------------
-- Spaces
-------------------------------------------------------------------------------

newtype Spaces = Spaces (Array Pos Space)

untagSpaces :: Spaces -> Array Pos Space
untagSpaces (Spaces arr) = arr

mkSpaces :: Players -> [Int] -> Spaces
mkSpaces (self, other) = Spaces 
  . array r 
  . zipWith tagSpace (range r)
  where 
    r = ((1, 1), (5, 5))
    tagSpace :: Pos -> Int -> (Pos, Space)
    tagSpace pos l
      | isPlayerAt self pos || isPlayerAt other pos = (pos, Occupied l)
      | otherwise  = (pos, Unoccupied l)

levelAt :: Spaces -> Pos -> Int
levelAt (Spaces arr) = untagSpace . (arr !)

neighbors :: Spaces -> Pos -> [(Pos, Space)]
neighbors (Spaces arr) p = filter (isNeighborOf p . fst) (Array.assocs  arr)
  where 
    isNeighborOf :: Pos -> Pos -> Bool
    isNeighborOf (x,y) (x',y') = 
      (x,y) /= (x',y')
      && abs (x' - x) <= 1 
      && abs (y' - y) <= 1

toLevels :: Spaces -> [[Int]]
toLevels = map (map (untagSpace . snd)) 
  . groupBy sameRow 
  . Array.assocs 
  . untagSpaces

to2dList :: Spaces -> [[Space]]
to2dList = map (map snd)
  . groupBy sameRow 
  . Array.assocs 
  . untagSpaces

sameRow :: (Pos, a) -> (Pos, a) -> Bool 
sameRow ((r, _), _) ((r', _), _) = r == r'

-------------------------------------------------------------------------------
-- Space
-------------------------------------------------------------------------------

data Space = Unoccupied Int | Occupied Int

untagSpace :: Space -> Int
untagSpace (Unoccupied l) = l
untagSpace (Occupied l) = l

isUnoccupied :: Space -> Bool
isUnoccupied (Unoccupied _) = True
isUnoccupied _ = False

-------------------------------------------------------------------------------
-- Pos
-------------------------------------------------------------------------------

type Pos = (Int, Int)

positions :: [Pos]
positions = range ((1,1), (5,5))

-------------------------------------------------------------------------------
-- Players
-------------------------------------------------------------------------------

data PlayerId = Self | Other

type Players = (Player, Player)

player :: PlayerId -> Players -> Player
player Self = getSelf
player Other = getOther

getSelf :: Players -> Player
getSelf = fst

getOther :: Players -> Player
getOther = snd

-------------------------------------------------------------------------------
-- Player
-------------------------------------------------------------------------------

type Player = [Pos]

isPlayerAt :: Player -> Pos -> Bool 
isPlayerAt =  flip elem
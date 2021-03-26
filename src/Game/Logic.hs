module Game.Logic where

import Control.Monad (guard)
import Game.State (GameState(..))
import Game.Tree (GameTree(..), minmax)
import Board (
    Pos
  , Token
  , Board(..)
  , Player(..)
  , PrePlayer(..)
  , Card (..)
  , Spaces(..)
  , Turn(..)
  , Move(..)
  , Build(..)
  , Space(..)
  , mkBoard
  , doTurn, boardPositions, levelAt, neighbors, unwrapSpace
  )

import Debug.Trace (trace)

-------------------------------------------------------------------------------
-- Turns
-------------------------------------------------------------------------------

-- | The maximal search depth in the game tree for taking a turn
searchDepth :: Int
searchDepth = 3

-- | Searches the game tree up to the `searchDepth` and picks an optimal turn
takeTurn :: GameState -> GameState
takeTurn s @ (OnePlaced _) = minmax (searchDepth - 1) s
takeTurn s = minmax searchDepth s

possibleTurns :: Board -> [Turn]
possibleTurns (Board turn players @ (Player Prometheus _, _) spaces) = do

  undefined
  -- (move @ (_, to), displace) <- possibleMoves players spaces

  -- build <- possibleBuilds (card . fst $ players) to spaces

  -- return (Turn (Just move) displace (Just build))

possibleTurns (Board turn players spaces) = do
  (move @ (_, to), displace) <- possibleMoves players spaces

  build <- possibleBuilds (card . fst $ players) to spaces

  return (Turn (Just move) displace (Just build))

possibleBuilds :: Card      -- The players card
                -> Pos      -- The position to build from
                -> Spaces   -- The current spaces
                -> [Build]  -- The possible builds
possibleBuilds Atlas build_from spaces = do
  (build_at, _) <- filter (canBuild . snd) . neighbors spaces $ build_from

  build_to <- [1 + levelAt spaces build_at, 4]

  return [(build_at, build_to)]
  where
    canBuild :: Space -> Bool
    canBuild (Unoccupied l) = l < 4
    canBuild (Occupied _) = False

possibleBuilds Demeter build_from spaces = do
  let neighs =  neighbors spaces build_from
  (build_at1, _) <- filter (canBuild . snd) neighs

  let build_to1 = 1 + levelAt spaces build_at1

  should_build_snd <- [True, False]

  if should_build_snd then do
    (build_at2, _) <- filter (canBuild . snd) . filter ((/= build_at1). fst) $ neighs

    let build_to2 = 1 + levelAt spaces build_at2

    return [(build_at1, build_to1), (build_at2, build_to2)]
  else
    return [(build_at1, build_to1)]
  where
    canBuild :: Space -> Bool
    canBuild (Unoccupied l) = l < 4
    canBuild (Occupied _) = False

possibleBuilds Hephastus build_from spaces = do
  (build_at, _) <- filter (canBuild . snd) . neighbors spaces $ build_from

  let build_at_level = levelAt spaces build_at

  build_to <- [build_at_level + 1, max (build_at_level + 2) 3]

  return [(build_at, build_to)]
  where
    canBuild :: Space -> Bool
    canBuild (Unoccupied l) = l < 4
    canBuild (Occupied _) = False

possibleBuilds _ build_from spaces = do
  (build_at, _) <- filter (canBuild . snd) . neighbors spaces $ build_from

  let build_to = 1 + levelAt spaces build_at

  return [(build_at, build_to)]
  where
    canBuild :: Space -> Bool
    canBuild (Unoccupied l) = l < 4
    canBuild (Occupied _) = False

possibleMoves :: (Player, Player )    -- The player
              -> Spaces               -- The current board spaces
              -> [(Move, Maybe Move)] -- The possible moves
possibleMoves (Player Apollo self_toks, Player _ other_toks) spaces = do
  from_pos <- self_toks

  let level = levelAt spaces from_pos

  (to_pos, to_space) <- filter (canMoveTo level . snd) . neighbors spaces $ from_pos

  case to_space of
    (Occupied _) -> return (
          (from_pos, to_pos)
        , Just (to_pos, from_pos)
      )
    (Unoccupied _) -> return (
          (from_pos, to_pos)
        , Nothing
      )
  where
    canMoveTo :: Int   -- current level
              -> Space -- the space to check
              -> Bool  -- whether the space can be moved to
    canMoveTo l (Occupied l') = levelReachable l l'
    canMoveTo l (Unoccupied l') = levelReachable l l'

possibleMoves (Player Minotaur self_toks, Player _ other_toks) spaces = do
  from_pos <- self_toks

  let level = levelAt spaces from_pos

  (to_pos, to_space) <- filter (canMoveTo level . snd) . neighbors spaces $ from_pos

  case to_space of
    (Occupied _) -> do
      -- If occupied then need to push opponent
      let to_level = levelAt spaces to_pos

      (push_to_pos, _) <- filter (canPushTo to_level . snd) . neighbors spaces $ to_pos

      -- allow for swap
      push_to_pos <- [push_to_pos, from_pos]

      return (
            (from_pos, to_pos)
          , Just (to_pos, push_to_pos)
        )
    (Unoccupied _) -> return (
          (from_pos, to_pos)
        , Nothing
      )
  where
    canMoveTo :: Int   -- current level
              -> Space -- the space to check
              -> Bool  -- whether the space can be moved to
    canMoveTo l (Occupied l') = levelReachable l l'
    canMoveTo l (Unoccupied l') = levelReachable l l'

    canPushTo :: Int   -- current level
              -> Space -- the space to check
              -> Bool  -- whether the space can be moved to
    canPushTo l (Occupied l') = False
    canPushTo l (Unoccupied l') = levelReachable l l'

possibleMoves (Player Artemis self_toks, _) spaces = do
  from_pos <- self_toks

  let level = levelAt spaces from_pos

  (to_pos, to_space) <- filter (canMoveTo level . snd) . neighbors spaces $ from_pos

  should_do_second_move <- [False, unwrapSpace to_space /= 3]

  if should_do_second_move then do
    let to_level = levelAt spaces to_pos

    (to_pos2, _) <- filter (canMoveTo to_level . snd) . neighbors spaces $ to_pos

    return (
        (from_pos, to_pos2)
      , Nothing
      )
  else
    return (
        (from_pos, to_pos)
      , Nothing
      )
  where
    canMoveTo :: Int   -- current level
              -> Space -- the space to check
              -> Bool  -- whether the space can be moved to
    canMoveTo l (Occupied _) = False
    canMoveTo l (Unoccupied l') = levelReachable l l'

possibleMoves (Player self_card self_toks, _) spaces = do
  from_pos <- self_toks

  let level = levelAt spaces from_pos

  (to_pos, _) <- filter (canMoveTo level . snd) . neighbors spaces $ from_pos

  return (
      (from_pos, to_pos)
    , Nothing
    )
  where
    canMoveTo :: Int   -- current level
              -> Space -- the space to check
              -> Bool  -- whether the space can be moved to
    canMoveTo l (Occupied _) = False
    canMoveTo l (Unoccupied l') = levelReachable l l'

levelReachable :: Int -> Int -> Bool
levelReachable from to = to < 4 && to <= from + 1

-------------------------------------------------------------------------------
-- GameState
-------------------------------------------------------------------------------

-- | The placement to preform if the first
firstPlacement :: [Token]
firstPlacement = [(1,1), (2,2)]

-- | A heuristic value of a board higher values denote move favorable game states
boardVal :: Board -> Int
boardVal _ = 0

instance GameTree GameState where
  isFinal (Won _)   = True
  isFinal (Lost _)  = True
  isFinal _         = False

  value (Won _)             = maxBound
  value (Lost _)            = minBound
  value (InProgress board)  = boardVal board
  value _                   = 0

  next (Won _)  = []
  next (Lost _) = []

  -- | Place according to firstPlacement
  next (NonePlaced (PrePlayer card,other)) = return $
    OnePlaced (other, Player card firstPlacement)

  -- | Pick two unoccupied spaces to place tokens
  next (OnePlaced (PrePlayer card, other @ (Player _ otherTokens))) =
    let
      unoccupied :: [Pos]
      unoccupied = filter (`notElem` otherTokens) boardPositions
    in do
      tok1 <- unoccupied
      tok2 <- unoccupied

      guard (tok1 /= tok2)

      return (BothPlaced (other, Player card [tok1, tok2]))

  -- Just convert to a board and get next for board
  next (BothPlaced players) = next
    . InProgress
    . mkBoard $ players

  -- All possible turns that the current player can make
  next (InProgress board) =
    case possibleTurns board of
      [] -> return (Lost board)
      ts -> do
        turn <- ts

        case doTurn board turn of
          (True, b') -> return (Won b')
          (False, b') -> return (InProgress b')

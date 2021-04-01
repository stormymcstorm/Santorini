-- {-# LANGUAGE BlockArguments #-}
module Logic.State where

import Control.Monad (guard, when)
import Debug.Trace (trace, traceM)
import Data.Tree.Game_tree.Game_tree ( Game_tree(..) )
-- import Data.Tree.Game_tree.Negascout ( negamax, negascout )
import qualified Data.Bifunctor as Bifunctor


import Logic.Tree

import Game.State

import Game.Spaces

import Game.Player

-------------------------------------------------------------------------------
-- GameState
-------------------------------------------------------------------------------

-- | The maximal search depth in the game tree for taking a turn
searchDepth :: Int
searchDepth = 5

-- | Searches the game tree up to the `searchDepth` and picks an optimal turn
takeTurn :: State -> State
-- takeTurn = takeTurnTraced'
takeTurn = takeTurn'

takeTurnTraced' :: State -> State
takeTurnTraced' s = trace ("pre-state:\n" ++ show s)
  (\s -> trace ("post-state:\n" ++ show s) s)
  $ takeTurn' s

takeTurn' :: State -> State
takeTurn' s @ (NonePlaced _) = negamax 1 s
takeTurn' s @ (OnePlaced _) = negamax 2 s
takeTurn' s @ (InProgress (Board turn _ _)) =
  (\d -> trace ("searching at depth: " ++ show d) negamax d s)
  . clamp 3 5 $ (turn * 4) `div` 30

clamp :: Int -> Int -> Int -> Int
clamp min max v
  | v > max   = max
  | v < min   = min
  | otherwise = v

instance GameTree State where
  isFinal (Won _)   = True
  isFinal (Lost _)  = True
  isFinal _         = False

  value (Won _)             = maxBound
  value (Lost _)            = minBound + 1
  value (InProgress board)  = boardVal board
  value _                   = 0

  next (Won _)  = []
  next (Lost _) = []

  -- | Place according to firstPlacement
  next (NonePlaced (PrePlayer self_card, other)) = return $
    OnePlaced (other, Player self_card firstPlacement)

  -- | Pick two unoccupied spaces to place tokens
  next (OnePlaced (PrePlayer self_card, other @ (Player _ other_tokens))) =
    let
      unoccupied :: [Pos]
      unoccupied = filter (`notElem` other_tokens) boardPositions
    in do
      tok1 <- unoccupied
      tok2 <- unoccupied

      guard (tok1 /= tok2)

      return $ BothPlaced (other, Player self_card [tok1, tok2])

  next (BothPlaced players) = return . InProgress $ mkInitialBoard players

  -- | Pick one of the possible turns
  next (InProgress board) =
    case possibleTurns board of
      [] -> return $ Won (swapPlayers board)
      ts -> do
        turn <- ts

        case doTurn board turn of
          (True, board) -> return $ Lost board
          (False, board) -> return $ InProgress board

boardVal :: Board -> Int
boardVal (Board _ (PlayerI _ self_toks, PlayerI _ other_toks) spaces) =
  sumTokenLevels self_toks - sumTokenLevels other_toks
  where
    sumTokenLevels :: [(Token, Level)] -> Int
    sumTokenLevels = sum . (>>= (\(pos, level) ->
      map ((\ l -> if l - level <= 1 && l <= 3 then level else 0) . spaceToLevel . snd)
      . neighbors spaces $ pos))

    sumNeighbors :: Pos -> Int
    sumNeighbors = sum . map (spaceToLevel . snd) . neighbors spaces

firstPlacement :: [Token]
firstPlacement = [(1,1), (2,2)]

-------------------------------------------------------------------------------
-- Turns
-------------------------------------------------------------------------------

possibleTurns :: Board -> [Turn]
possibleTurns (Board _ (self, _) spaces) = possibleTurns' self spaces

possibleTurns' :: PlayerI -> Spaces -> [Turn]

-- | Apollo can optionally swap places with an opponent token
possibleTurns' (PlayerI Apollo tokens) spaces = do
  (from, from_l) <- tokens

  (to, to_s) <- filter (canMoveTo from_l . snd)
    . neighbors spaces $ from

  let (displace, to_l) = case to_s of
        Unoccupied l -> (Nothing, l)
        Other l -> (Just (to, from), l)

  if to_l == 3 && from_l /= 3 then
    return $ Turn (from, to) displace Nothing
  else do
    (build_at, build_l) <- case displace of
      Nothing -> (from, from_l) : (
        map (Bifunctor.second spaceToLevel)
        . filter (canBuildAt . snd)
        . neighbors spaces
        $ to)
      Just _ ->
        map (Bifunctor.second spaceToLevel)
        . filter (canBuildAt . snd)
        . neighbors spaces $ to

    return $ Turn (from, to) displace (Just [(build_at, build_l + 1)])
  where
    canMoveTo :: Int -> Space -> Bool
    canMoveTo l (Unoccupied l') = isReachable l l'
    canMoveTo l (Other l')      = isReachable l l'
    canMoveTo _ _               = False

    canBuildAt :: Space -> Bool
    canBuildAt (Unoccupied l) = l < 4
    canBuildAt _              = False


-- Artemis can optionally move the same token a second time
possibleTurns' (PlayerI Artemis tokens) spaces = do
  (from, from_l) <- tokens

  (to, to_s) <- filter (canMoveTo from_l . snd)
    . neighbors spaces $ from

  let to_l = spaceToLevel to_s

  if to_l == 3 && from_l /= 3 then
    return $ Turn (from, to) Nothing Nothing
  else do
    should_move_again <- [False, True]

    if should_move_again then do
      (to2, to2_s) <- filter (canMoveTo to_l . snd)
        . neighbors spaces $ to

      guard (from /= to2)

      let to2_l = spaceToLevel to2_s

      if to2_l == 3 then
        return $ Turn (from, to2) Nothing Nothing
      else do
        (build_at, build_s) <- filter (canBuildAt . snd)
          . neighbors spaces $ to2

        let build_l = 1 + spaceToLevel build_s

        return $ Turn (from, to2) Nothing (Just [(build_at, build_l)])
    else do
      (build_at, build_l) <- (from, from_l) : (
        map (Bifunctor.second spaceToLevel)
        . filter (canBuildAt . snd)
        . neighbors spaces
        $ to)

      -- when (to == (1,3)) (traceM ("build_at:" ++ show build_at))

      -- let build_l = 1 + spaceToLevel build_s

      return $ Turn (from, to) Nothing (Just [(build_at, build_l + 1)])
  where
    canMoveTo :: Int -> Space -> Bool
    canMoveTo l (Unoccupied l') = isReachable l l'
    canMoveTo _ _               = False

    canBuildAt :: Space -> Bool
    canBuildAt (Unoccupied l) = l < 4
    canBuildAt _              = False

-- Atlas
possibleTurns' (PlayerI Atlas tokens) spaces = do
  (from, from_l) <- tokens

  (to, to_s) <- filter (canMoveTo from_l . snd)
    . neighbors spaces $ from

  let to_l = spaceToLevel to_s

  if to_l == 3 && from_l /= 3 then
    return $ Turn (from, to) Nothing Nothing
  else do
    (build_at, build_l) <- (from, from_l) : (
      map (Bifunctor.second spaceToLevel)
      . filter (canBuildAt . snd)
      . neighbors spaces $ to)

    build_to <- [1 + build_l, 4]

    return $ Turn (from, to) Nothing (Just [(build_at, build_to)])
  where
    canMoveTo :: Int -> Space -> Bool
    canMoveTo l (Unoccupied l') = isReachable l l'
    canMoveTo _ _               = False

    canBuildAt :: Space -> Bool
    canBuildAt (Unoccupied l) = l < 4
    canBuildAt _              = False

-- Pan
possibleTurns' (PlayerI Demeter tokens) spaces = do
  (from, from_l) <- tokens

  (to, to_s) <- filter (canMoveTo from_l . snd)
    . neighbors spaces $ from

  let to_l = spaceToLevel to_s

  if (to_l == 3 && from_l /= 3) || from_l - to_l >= 2 then
    return $ Turn (from, to) Nothing Nothing
  else do
    let to_neighs = neighbors spaces to
    (build1_at, build1_l) <- (from, from_l) : (
      map (Bifunctor.second spaceToLevel)
      . filter (canBuildAt . snd) $ to_neighs)

    let build1_to = build1_l + 1

    build_again <- [True, False]

    if build_again then do
      (build2_at, build2_l) <- (from, from_l) : (
        map (Bifunctor.second spaceToLevel)
        . filter (canBuildAt . snd) $ to_neighs)

      guard (build2_at /= build1_at)

      let build2_to = build2_l + 1

      return $ Turn (from, to) Nothing (Just [(build1_at, build1_to), (build2_at, build2_to)])
    else
      return $ Turn (from, to) Nothing (Just [(build1_at, build1_to)])
  where
    canMoveTo :: Int -> Space -> Bool
    canMoveTo l (Unoccupied l') = isReachable l l'
    canMoveTo _ _               = False

    canBuildAt :: Space -> Bool
    canBuildAt (Unoccupied l) = l < 4
    canBuildAt _              = False

-- Hephastus
possibleTurns' (PlayerI Hephastus tokens) spaces = do
  (from, from_l) <- tokens

  (to, to_s) <- filter (canMoveTo from_l . snd)
    . neighbors spaces $ from

  let to_l = spaceToLevel to_s

  if to_l == 3 && from_l /= 3 then
    return $ Turn (from, to) Nothing Nothing
  else do

    (build_at, build_l) <- (from, from_l) : (
      map (Bifunctor.second spaceToLevel)
      . filter (canBuildAt . snd)
      . neighbors spaces $ to)

    build_to <- if build_l < 2 then
        [build_l + 1, build_l + 2]
      else
        [build_l + 1]

    return $ Turn (from, to) Nothing (Just [(build_at, build_to)])
  where
    canMoveTo :: Int -> Space -> Bool
    canMoveTo l (Unoccupied l') = isReachable l l'
    canMoveTo _ _               = False

    canBuildAt :: Space -> Bool
    canBuildAt (Unoccupied l) = l < 4
    canBuildAt _              = False

-- Minotaur
possibleTurns' (PlayerI Minotaur tokens) spaces = do
  (from, from_l) <- tokens

  (to, to_s) <- filter (canMoveTo from_l . snd)
    . neighbors spaces $ from

  case to_s of
    Unoccupied to_l -> do
      if to_l == 3 && from_l /= 3 then
        return $ Turn (from, to) Nothing Nothing
      else do
        (build_at, build_l) <- (from, from_l) : (
          map (Bifunctor.second spaceToLevel)
          . filter (canBuildAt . snd)
          . neighbors spaces
          $ to)

        return $ Turn (from, to) Nothing (Just [(build_at, build_l + 1)])

    Other to_l -> do
      let push_to @ (push_to_x, push_to_y) = pushTo from to

      guard (push_to_x >= 1 && push_to_x <= 5
        && push_to_y >= 1 && push_to_y <= 5)

      let push_to_s = spaceAt spaces push_to

      guard (spaceToLevel push_to_s < 4 && isUnoccupied push_to_s)

      let displace = Just (to, push_to)

      if to_l == 3 && from_l /= 3 then
        return $ Turn (from, to) displace Nothing
      else if push_to /= from then do
        (build_at, build_l) <- (from, from_l) : (
          map (Bifunctor.second spaceToLevel)
          . filter (canBuildAt . snd)
          . neighbors spaces
          $ to)

        guard (build_at /= push_to)

        -- return $ Turn (from, to) displace Nothing
        return $ Turn (from, to) displace (Just [(build_at, build_l + 1)])
      else do
        (build_at, build_l) <- map (Bifunctor.second spaceToLevel)
          . filter (canBuildAt . snd)
          . neighbors spaces
          $ to

        -- guard (build_at /= push_to)

        return $ Turn (from, to) displace (Just [(build_at, build_l + 1)])
  where
    canMoveTo :: Int -> Space -> Bool
    canMoveTo l (Unoccupied l') = isReachable l l'
    canMoveTo l (Other l')      = isReachable l l'
    canMoveTo _ _               = False

    canBuildAt :: Space -> Bool
    canBuildAt (Unoccupied l) = l < 4
    canBuildAt _              = False

    canPushTo :: Space -> Bool
    canPushTo (Unoccupied l) = l < 4
    canPushTo _               = False

    pushTo :: Pos -> Pos -> Pos
    pushTo (from_x, from_y) (to_x, to_y) = (to_x * 2 - from_x, 2 * to_y - from_y)

-- Pan
possibleTurns' (PlayerI Pan tokens) spaces = do
  (from, from_l) <- tokens

  (to, to_s) <- filter (canMoveTo from_l . snd)
    . neighbors spaces $ from

  let to_l = spaceToLevel to_s

  if (to_l == 3 && from_l /= 3) || from_l - to_l >= 2 then
    return $ Turn (from, to) Nothing Nothing
  else do
    (build_at, build_l) <- (from, from_l) : (
      map (Bifunctor.second spaceToLevel)
      . filter (canBuildAt . snd)
      . neighbors spaces $ to)

    return $ Turn (from, to) Nothing (Just [(build_at, build_l + 1)])
  where
    canMoveTo :: Int -> Space -> Bool
    canMoveTo l (Unoccupied l') = isReachable l l'
    canMoveTo _ _               = False

    canBuildAt :: Space -> Bool
    canBuildAt (Unoccupied l) = l < 4
    canBuildAt _              = False

-- Prometheus
possibleTurns' (PlayerI Prometheus tokens) spaces = do
  (from, from_l) <- tokens

  build_before_move <- [True, False]

  if build_before_move then do
    let from_neighs = neighbors spaces from

    (build1_at, build1_l) <-
        map (Bifunctor.second spaceToLevel)
        . filter (canBuildAt . snd) $ from_neighs

    let build1_to = build1_l + 1

    (to, to_l) <-
      if build1_to == from_l then
        (build1_at, build1_to) : (
          map (Bifunctor.second spaceToLevel)
          . filter ((==Unoccupied from_l) . snd) $ from_neighs)
      else
          map (Bifunctor.second spaceToLevel)
          . filter (\(p,s) -> p /= build1_at && s == Unoccupied from_l) $ from_neighs

    (build2_at, build2_l) <- (from, from_l) : (
        map (Bifunctor.second spaceToLevel)
        . filter (canBuildAt . snd)
        . neighbors spaces $ to)

    let build2_to = build2_l + 1

    return $ Turn (from, to) Nothing (Just [(build1_at, build1_to), (build2_at, build2_to)])
  else do
    (to, to_s) <- filter (canMoveTo from_l . snd)
      . neighbors spaces $ from

    let to_l = spaceToLevel to_s

    if to_l == 3 && from_l /= 3then
      return $ Turn (from, to) Nothing Nothing
    else do
      (build_at, build_l) <- (from, from_l) : (
        map (Bifunctor.second spaceToLevel)
        . filter (canBuildAt . snd)
        . neighbors spaces $ to)

      return $ Turn (from, to) Nothing (Just [(build_at, build_l + 1)])
  where
    canMoveTo :: Int -> Space -> Bool
    canMoveTo l (Unoccupied l') = isReachable l l'
    canMoveTo _ _               = False

    canBuildAt :: Space -> Bool
    canBuildAt (Unoccupied l) = l < 4
    canBuildAt _              = False


isReachable :: Int -> Int -> Bool
isReachable from to = to < 4 && to <= from + 1

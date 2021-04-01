{-# LANGUAGE TupleSections #-}
module Game.State where

import Data.Array (Array, array, (!), (//))
import qualified Data.Bifunctor as Bifunctor

import Debug.Trace (trace)

import Game.Player

import Game.Spaces
import Data.List (intercalate)

-------------------------------------------------------------------------------
-- GameState
-------------------------------------------------------------------------------

data State =
    NonePlaced (PrePlayer, PrePlayer) -- ^ Where neither player has placed their tokens
  | OnePlaced (PrePlayer, Player) -- ^ Where one player has placed their tokens
  | BothPlaced (Player, Player) -- ^ Where both players have placed their tokens
  | InProgress Board -- ^ An inprogress game
  | Won Board -- ^ Where the active player has lost
  | Lost Board -- ^ Where the active player has won
  deriving (Eq)

instance Show State where
  show (NonePlaced (self, other)) =
    show self ++ " " ++ show other
  show (OnePlaced (self, other)) =
    show self ++ " " ++ show other
  show (BothPlaced (self, other)) =
    show self ++ " " ++ show other
  show (InProgress board @ (Board turn _ _ )) = "InProgress @ turn " ++ show turn ++ "\n"  ++ show board
  show (Won board  @ (Board turn _ _ )) = "Won @ turn " ++ show turn ++ "\n"  ++ show board
  show (Lost board  @ (Board turn _ _ )) = "Lost @ turn " ++ show turn ++ "\n"  ++ show board

-------------------------------------------------------------------------------
-- Turn
-------------------------------------------------------------------------------

data Turn = Turn Move (Maybe Move) (Maybe Build)

doTurn :: Board -> Turn -> (Bool, Board)
doTurn (Board tn (self, other) spaces)
        (Turn (move_from, move_to) Nothing Nothing) =
  let
    move_from_l = levelAt spaces move_from
    move_to_l = levelAt spaces move_to

    self_toks' @ [self_tok1', self_tok2'] =
        let
          [tok1, tok2] = tokens self
        in
          if fst tok1 == move_from then
            [(move_to, move_to_l), tok2]
          else
            [(move_to, move_to_l), tok1]

    [other_tok1', other_tok2'] = tokens other

    players' = (
          other
        , self {tokens = self_toks'}
      )

    update :: [(Pos, Space)]
    update = [
          (move_from, Unoccupied move_from_l)
        , Bifunctor.second Other self_tok1'
        , Bifunctor.second Other self_tok2'
        , Bifunctor.second Self other_tok1'
        , Bifunctor.second Self other_tok2'
      ]

    is_win = case card self of
        Pan -> (move_to_l == 3 &&  move_from_l /= 3) || (move_from_l - move_to_l >= 2)
        _   -> move_to_l == 3 &&  move_from_l /= 3

    spaces' = updateSpaces update spaces
  in (is_win, Board (tn + 1) players' spaces')

doTurn (Board tn (self, other) spaces)
        (Turn (move_from, move_to) Nothing (Just build)) =
  let
    move_from_l = levelAt spaces move_from
    move_to_l = levelAt spaces move_to

    self_toks' @ [self_tok1', self_tok2'] =
        let
          [tok1, tok2] = tokens self
        in
          if fst tok1 == move_from then
            [(move_to, move_to_l), tok2]
          else
            [(move_to, move_to_l), tok1]

    [other_tok1', other_tok2'] = tokens other

    -- [(build1_at, build1_to), (build2_at, build2_to)] = build

    players' = (
          other
        , self {tokens = self_toks'}
      )

    build_update :: [(Pos, Space)]
    build_update = map (Bifunctor.second Unoccupied) build

    update :: [(Pos, Space)]
    update
      | move_from `elem` map fst build =
        [
              Bifunctor.second Other self_tok1'
            , Bifunctor.second Other self_tok2'
            , Bifunctor.second Self other_tok1'
            , Bifunctor.second Self other_tok2'
        ] ++ build_update
      | otherwise =
        [
              (move_from, Unoccupied move_from_l)
            , Bifunctor.second Other self_tok1'
            , Bifunctor.second Other self_tok2'
            , Bifunctor.second Self other_tok1'
            , Bifunctor.second Self other_tok2'
        ] ++ build_update

    is_win = case card self of
        Pan -> (move_to_l == 3 &&  move_from_l /= 3) || (move_from_l - move_to_l >= 2)
        _   -> move_to_l == 3 &&  move_from_l /= 3

    spaces' = updateSpaces update spaces
  in (is_win, Board (tn + 1) players' spaces')

doTurn (Board tn (self, other) spaces)
        (Turn (move_from, move_to) (Just (disp_from, disp_to)) Nothing)
  | disp_from /= move_to =
    error "disp_from /= move_to"
  | otherwise =
    let
      move_from_l = levelAt spaces move_from
      move_to_l = levelAt spaces move_to
      disp_to_l = levelAt spaces disp_to

      self_toks' @ [self_tok1', self_tok2'] =
        let
          [tok1, tok2] = tokens self
        in
          if fst tok1 == move_from then
            [(move_to, move_to_l), tok2]
          else
            [(move_to, move_to_l), tok1]

      other_toks' @ [other_tok1', other_tok2'] =
        let
          [tok1, tok2] = tokens other
        in
          if fst tok1 == disp_from then
            [(disp_to, disp_to_l), tok2]
          else
            [(disp_to, disp_to_l), tok1]

      players' = (
            other {tokens = other_toks'}
          , self {tokens = self_toks'}
        )

      update :: [(Pos, Space)]
      update
        | disp_to == move_from =
          [
              Bifunctor.second Other self_tok1'
            , Bifunctor.second Other self_tok2'
            , Bifunctor.second Self other_tok1'
            , Bifunctor.second Self other_tok2'
          ]
        | otherwise =
          [
              (move_from, Unoccupied move_from_l)
            , Bifunctor.second Other self_tok1'
            , Bifunctor.second Other self_tok2'
            , Bifunctor.second Self other_tok1'
            , Bifunctor.second Self other_tok2'
          ]

      is_win = case card self of
          Pan -> (move_to_l == 3 &&  move_from_l /= 3) || (move_from_l - move_to_l >= 2)
          _   -> move_to_l == 3 &&  move_from_l /= 3

      spaces' = updateSpaces update spaces
    in (is_win, Board (tn + 1) players' spaces')

doTurn (Board tn (self, other) spaces)
        (Turn (move_from, move_to) (Just (disp_from, disp_to)) (Just build))
  | disp_from /= move_to =
    error "disp_from /= move_to"
  | otherwise =
    let
      move_from_l = levelAt spaces move_from
      move_to_l = levelAt spaces move_to
      disp_to_l = levelAt spaces disp_to

      self_toks' @ [self_tok1', self_tok2'] =
        let
          [tok1, tok2] = tokens self
        in
          if fst tok1 == move_from then
            [(move_to, move_to_l), tok2]
          else
            [(move_to, move_to_l), tok1]

      other_toks' @ [other_tok1', other_tok2'] =
        let
          [tok1, tok2] = tokens other
        in
          if fst tok1 == disp_from then
            [(disp_to, disp_to_l), tok2]
          else
            [(disp_to, disp_to_l), tok1]

      players' = (
            other {tokens = other_toks'}
          , self {tokens = self_toks'}
        )

      build_update :: [(Pos, Space)]
      build_update = map (Bifunctor.second Unoccupied) build

      update :: [(Pos, Space)]
      update
        | move_from `elem` map fst build || disp_to == move_from =
          [
              Bifunctor.second Other self_tok1'
            , Bifunctor.second Other self_tok2'
            , Bifunctor.second Self other_tok1'
            , Bifunctor.second Self other_tok2'
          ] ++ build_update
        | otherwise =
          [
              (move_from, Unoccupied move_from_l)
            , Bifunctor.second Other self_tok1'
            , Bifunctor.second Other self_tok2'
            , Bifunctor.second Self other_tok1'
            , Bifunctor.second Self other_tok2'
          ] ++ build_update

      is_win = case card self of
          Pan -> (move_to_l == 3 &&  move_from_l /= 3) || (move_from_l - move_to_l >= 2)
          _   -> move_to_l == 3 &&  move_from_l /= 3

      spaces' = updateSpaces update spaces
    in (is_win, Board (tn + 1) players' spaces')

-------------------------------------------------------------------------------
-- Move
-------------------------------------------------------------------------------

type Move = (Pos, Pos)

-------------------------------------------------------------------------------
-- Build
-------------------------------------------------------------------------------

type Build = [(Pos, Int)]


-------------------------------------------------------------------------------
-- Board
-------------------------------------------------------------------------------

data Board = Board {turn:: !Int, players:: !(PlayerI, PlayerI), spaces :: Spaces}
  deriving (Eq)

swapPlayers :: Board -> Board
swapPlayers (Board turn (self, other) spaces) = Board (turn + 1) (other, self) spaces

mkBoard :: Int -> (Player, Player) -> [Int] -> Board
mkBoard turn players levels =
  let
    (spaces, players') = mkSpaces players levels
  in Board turn players' spaces

mkInitialBoard :: (Player, Player) -> Board
mkInitialBoard players = mkBoard 0 players (replicate 25 0)

instance Show Board where
  show (Board _ _ spaces) = show spaces


-------------------------------------------------------------------------------
-- PlayerI
-------------------------------------------------------------------------------

-- | A player that has placed their tokens
data PlayerI = PlayerI {
    card :: !Card
  , tokens :: ![(Pos, Int)]
  }
  deriving (Eq)

toPlayerI :: Player -> PlayerI
toPlayerI (Player card tokens) = PlayerI card (map (, 0) tokens)

toPlayer :: PlayerI -> Player
toPlayer (PlayerI card tokens) = Player card (map fst tokens)

-- | Returns true if and only if the player has a token at the given position.
isPlayerAt :: PlayerI    -- ^ The player whose tokens to check
          -> (Int, Int) -- ^ The position to check for
          -> Bool       -- ^ Whether the player has a token at the given position
isPlayerAt = flip elem . map fst . tokens


instance Show PlayerI where
  show (PlayerI card tokens) =
    show card
    ++ "@"
    ++ (intercalate " & " . map show $ tokens)

-------------------------------------------------------------------------------
-- PlayerI
-------------------------------------------------------------------------------


-- | Constructs the spaces from the given players and levels
mkSpaces :: (Player, Player) -> [Level] -> (Spaces, (PlayerI, PlayerI))
mkSpaces (self @ (Player self_card self_toks), other @ (Player other_card other_toks)) =
  fork id mkPlayers . Spaces
  . array posRange
  . zipWith tagSpace boardPositions
  where
    mkPlayers :: Spaces -> (PlayerI, PlayerI)
    mkPlayers spaces = (
          PlayerI self_card (map (fork id (levelAt spaces)) self_toks)
        , PlayerI other_card  (map (fork id (levelAt spaces)) other_toks)
      )

    tagSpace :: Pos -> Level -> (Pos, Space)
    tagSpace pos level
      | isPlayerAt' self pos   = (pos, Self level)
      | isPlayerAt' other pos  = (pos, Other level)
      | otherwise             = (pos, Unoccupied level)

isPlayerAt' :: Player    -- ^ The player whose tokens to check
          -> (Int, Int) -- ^ The position to check for
          -> Bool       -- ^ Whether the player has a token at the given position
isPlayerAt' (Player _ tokens) pos = pos `elem` tokens

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

replaceIf :: (a -> Bool) -> a -> [a] -> [a]
replaceIf pred with = map (\e -> if pred e then with else e)

fork :: (a -> c) -> (a -> d) -> a -> (c, d)
fork f g v = (f v, g v)

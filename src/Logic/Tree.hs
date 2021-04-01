module Logic.Tree (negamax, abPrune, GameTree(..)) where

import qualified Data.Bifunctor as Bifunctor
import Debug.Trace (trace)
import Data.List (sortBy)

-------------------------------------------------------------------------------
-- GameTree
-------------------------------------------------------------------------------

class Show a => GameTree a where
  -- | Whether the game tree node is final. The implementor must meet the condition
  --    that `isFinal a ==> next a = []`
  isFinal :: a -> Bool

  -- | A heuristic denoting that advantage the active player has over the other
  value :: a -> Int

  -- | Returns a list of the reachable states from the current state
  next :: a -> [a]

-------------------------------------------------------------------------------
-- negamax
-------------------------------------------------------------------------------

negamax :: GameTree a => Int -> a -> a
negamax depth = fst . negamax' depth (-1) (minBound + 1) maxBound

negamax' :: GameTree a =>
           Int      -- ^ The depth to search to
        -> Int      -- ^ The color, either 1 for self and -1 for other
        -> Int
        -> Int
        -> a        -- ^ The tree to search
        -> (a, Int) -- ^ The best possible next state and it's value
negamax' 0 color _ _ node = (node, color * value node)
negamax' depth color alpha beta node
  | isFinal node =
    let
      val = color * value node
    in
      -- trace ("final at depth " ++ show depth
      --   ++ ", with value " ++ show val
      --   ++ ", with color " ++ show color
      --   ++ ":\n" ++ show node)
      (node, val)
  | otherwise =
    let
    in
      -- (\x -> if depth >= 3 then trace ("picked:\n" ++ show (fst x) ++ "at depth " ++ show depth) x else x) .
      fst
      . last
      . takeWhile (\(_, alpha') -> alpha' < beta)
      . (\(c:cs) -> scanl findMax ((c, findVal alpha c), alpha) cs)
      . map fst
      . sortBy (\(_, v) (_, v') -> compare v v')
      . map (\n -> (n, value n))
      $ next node
    where
      findMax :: GameTree a => ((a, Int), Int) -> a -> ((a, Int), Int)
      findMax ((n, max), alpha') n' =
        let
          v = findVal alpha' n'
        in (if v > max then (n', v) else (n, max), if v > alpha' then v else alpha')

      findVal :: GameTree a =>
                 Int
              -> a   -- ^ The node whose value to find
              -> Int -- ^ The value of the node
      findVal alpha' = negate
        . snd
        . negamax' (depth - 1) (-color) (-beta) (-alpha')

-------------------------------------------------------------------------------
-- abPrune
-------------------------------------------------------------------------------

abPrune :: GameTree a => Int -> a -> a
abPrune d = snd . abPruneMax d minBound maxBound

abPruneMax :: GameTree a => Int
          -> Int -- alpha
          -> Int -- beta
          -> a
          -> (Int, a)
abPruneMax 0 _ _ node = (value node, node)
abPruneMax depth alpha beta node
  | isFinal node  = (value node, node)
  | otherwise     =
    let
      (n:ns) = next node
    in fst
      . last
      . takeWhile (\(_, alpha') -> alpha' < beta)
      . scanl updateMax (doMin alpha n, alpha)
      $ ns
    where
      doMin :: GameTree a => Int -> a -> (Int, a)
      doMin alpha' n = (fst $ abPruneMin (depth - 1) alpha' beta n, n)

      updateMax :: GameTree a => ((Int, a), Int) -> a -> ((Int, a), Int)
      updateMax (c_max @ (v, _), c_alpha) nn =
        let
          nn_min @ (v', _) = doMin c_alpha nn
        in (if v' > v then nn_min else c_max, if v' > c_alpha then v' else c_alpha)

abPruneMin :: GameTree a => Int
          -> Int -- alpha
          -> Int -- beta
          -> a
          -> (Int, a)
abPruneMin 0 _ _ node = (value node, node)
abPruneMin depth alpha beta node
  | isFinal node = (value node, node)
  | otherwise     =
  let
    (n:ns) = next node
  in
    fst
    . last
    . takeWhile (\(_, beta') -> beta' > alpha)
    . scanl updateMin (doMax beta n, beta)
    $ ns
  where
    doMax :: GameTree a => Int -> a -> (Int, a)
    doMax beta' n = (fst $ abPruneMax (depth - 1) alpha beta' n, n)

    updateMin :: GameTree a => ((Int, a), Int) -> a -> ((Int, a), Int)
    updateMin (c_min @ (v, _), c_beta) nn =
      let
        nn_max @ (v', _) = doMax c_beta nn
      in (if v' < v then nn_max else c_min, if v' < c_beta then v' else c_beta)

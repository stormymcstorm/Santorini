module Game.Tree where

-------------------------------------------------------------------------------
-- GameTree
-------------------------------------------------------------------------------

class GameTree a where
  isFinal :: a -> Bool
  value :: a -> Int 
  next :: a -> [a]

minmax :: GameTree a => Int -> a -> a
minmax d = snd . maximize d

maximize :: GameTree a => Int -> a -> (Int, a)
maximize 0 node = (-value node, node)
maximize depth node
  | isFinal node = (-value node, node)
  | otherwise = max . map (\n -> (fst (minimize (depth - 1) n), n)) $ next node 
  where 
    max :: [(Int, a)] -> (Int, a)
    max = foldl1 (\c_max @ (v, _) nn @ (v', _) -> if v' > v then nn else c_max)

minimize :: GameTree a => Int -> a -> (Int, a)
minimize 0 node = (value node, node)
minimize depth node
  | isFinal node = (value node, node)
  | otherwise = min . map (\n -> (fst (maximize (depth - 1) n), n)) $ next node 
  where 
    min :: [(Int, a)] -> (Int, a)
    min = foldl1 (\c_min @ (v, _) nn @ (v', _) -> if v' < v then nn else c_min)

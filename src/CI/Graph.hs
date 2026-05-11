module CI.Graph (reachable) where

import qualified Data.Set as Set

reachable :: (Ord a) => (a -> [a]) -> a -> Set.Set a
reachable next root = bfs Set.empty [root]
  where
    bfs seen [] = seen
    bfs seen (x : xs)
      | Set.member x seen = bfs seen xs
      | otherwise = bfs (Set.insert x seen) (next x <> xs)

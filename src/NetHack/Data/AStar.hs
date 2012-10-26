module NetHack.Data.AStar
  (findPathTo)
  where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe(fromJust)

type NeighbourCoordinatesFun a = (a -> [a])
type DistFun a = (a -> a -> Int)

-- Despite the module name, this is actually a breadth first search instead
-- of A* search.
findPathTo :: (Eq a, Ord a) =>
              NeighbourCoordinatesFun a ->
              a ->
              a ->
              Maybe [a]
findPathTo neighbourFun start end =
  accum M.empty
        (S.singleton start)
        [start]
        S.empty 0
  where
    accum cameFrom visited [] nextqueue d
      | nextqueue == S.empty = Nothing
      | otherwise =
           accum cameFrom visited (S.toList nextqueue) S.empty (d+1)
    accum cameFrom visited (q:queue) nextqueue d
      | q == end = reConstructPath cameFrom end [end]
      | otherwise =
          let neighbours = neighbourFun q
              newVisited = S.insert q visited
              newCameFrom =
                foldl (\cameFrom coord -> if S.notMember coord newVisited &&
                                             M.notMember coord cameFrom
                                            then M.insert coord q cameFrom
                                            else cameFrom)
                      cameFrom neighbours
           in accum newCameFrom
                    newVisited
                    queue
                    (foldl (\nextqueue coord -> if S.notMember coord newVisited
                                                  then S.insert coord nextqueue
                                                  else nextqueue)
                           nextqueue neighbours)
                    d

    reConstructPath cameFrom current accum =
      case M.lookup current cameFrom of
        Nothing -> Just (tail accum)  -- tail drops the first one
        Just co -> reConstructPath cameFrom co (co:accum)



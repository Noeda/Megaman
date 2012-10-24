module NetHack.Data.AStar
  (findPathTo)
  where

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe(fromJust)

type NeighbourCoordinatesFun a = (a -> [a])
type DistFun a = (a -> a -> Int)

data AStar a = AStar { closedSet :: S.Set a,
                       openSet :: M.Map Int a,
                       openSetS :: S.Set a,
                       cameFrom :: M.Map a a,
                       gScore :: M.Map a Int,
                       fScore :: M.Map a Int }

findPathTo :: forall a. Ord a =>
              NeighbourCoordinatesFun a ->
              DistFun a ->
              a ->
              a ->
              Maybe [a]
findPathTo neighbourFun dist start end =
  accum (AStar S.empty
               (M.singleton 0 start)
               (S.singleton start)
               M.empty
               (M.singleton start 0)
               (M.singleton start $ dist start end))
  where
   accum :: Ord a => AStar a -> Maybe [a]
   accum state
     | M.null (openSet state) = Nothing
     | otherwise =
         let (key, next) = M.findMin (openSet state)
             coordFun = (\coords -> S.notMember coords (closedSet state))
          in if next == end
               then reconstructPath (cameFrom state) end
               else addNeighbours
                      (state { closedSet = S.insert next (closedSet state),
                               openSet = M.delete key (openSet state),
                               openSetS = S.delete next (openSetS state) })
                      next
                      (filter coordFun $ neighbourFun next)


   addNeighbours :: Eq a =>
                    AStar a -> a -> [a] -> Maybe [a]
   addNeighbours state _ [] = accum state
   addNeighbours state next (neighbour:rest)
     | S.notMember neighbour ((openSetS state) :: S.Set a) ||
       tentativeGScore <= fromJust (M.lookup neighbour $ gScore state)
         = addNeighbours
             (state { cameFrom = M.insert neighbour next (cameFrom state),
                      gScore = M.insert neighbour tentativeGScore
                               (gScore state),
                      fScore = M.insert neighbour fscore (fScore state),
                      openSetS = S.insert neighbour (openSetS state),
                      openSet = M.insert fscore neighbour (openSet state) })
             next rest
    | otherwise = addNeighbours state next rest
     where
       tentativeGScore = fromJust (M.lookup next (gScore state))
                       + dist next neighbour
       fscore = tentativeGScore + dist neighbour end

   reconstructPath :: M.Map a a -> a -> Maybe [a]
   reconstructPath cameFrom current = Just $
                                      reconstructPath2 cameFrom current []
   reconstructPath2 cameFrom current accum
     | M.member current cameFrom =
         reconstructPath2 cameFrom (fromJust $ M.lookup current cameFrom)
            (current:accum)
     | otherwise = accum



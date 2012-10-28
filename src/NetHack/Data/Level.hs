module NetHack.Data.Level
  (initialElement,
   Level(),
   newLevel,
   feature,
   elements,
   setElements,
   setItems,
   items,
   lookedLike,
   Element(),
   Feature(..),
   weirdAppearance,
   featureByStr,
   featureByCh,
   elemAt, elemAtDefault,
   updateElement,
   featureAt,
   setAppearance,
   setBoulder,
   setFeature,
   setStairsLevel,
   removeBoulder,
   removeMonster,
   setMonsterInstance,
   setUnexploredItems,
   reachablePositions,
   explorableReachablePositions,
   isDungeonFeature,
   levelCoordinates,
   levelCoordinatesExcept,
   sortByDistance,
   findPathTo,
   forbidMovementFrom,
   Coords(..),
   neighbourCoordinates,
   isNextTo,
   levelID,
   findClosedDoors,
   findDownstairs,
   maybeMarkAsOpenDoor)
  where

import NetHack.Data.MonsterInstance(MonsterInstance)
import Terminal.Data(Attributes, Color(..), foreground, bold, defaultAttributes)

import Data.Array(Array, array)
import Data.List((\\), sortBy, intersect)

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Terminal.Data as T

import qualified NetHack.Data.AStar as AStar

import Data.Maybe(fromJust)
import Control.Monad

import NetHack.Data.Alignment
import NetHack.Data.Item

import NetHack.Data.Appearance

type Coords = (Int, Int)

data Level = Level { number    :: Int,
                     levelID   :: LevelID,
                     elements  :: M.Map Coords Element,
                     forbiddenMoves :: S.Set (Coords, Coords),
                     endGame   :: Bool }
                     deriving(Show)

data Element = Element { searched   :: Int,
                         walked     :: Int,
                         diggable   :: Bool,
                         boulder    :: Bool,
                         lookedLike :: Appearance,
                         unexploredItems :: Bool,
                         items      :: M.Map (Maybe Char) [Item],
                         monster    :: Maybe MonsterInstance,
                         feature    :: Maybe Feature }
                       deriving(Show, Eq)

type LevelID = Int

data Feature = DownStairs (Maybe LevelID) |
               UpStairs   (Maybe LevelID) |
               DownLadder (Maybe LevelID) |
               UpLadder   (Maybe LevelID) |
               Portal     (Maybe LevelID) |
               Throne           |
               Floor            |
               UnknownFloor     |  -- Just like Floor but
                                   -- 'is not known for sure it is floor'
               Wall             |
               ClosedDoor       |
               OpenedDoor       |
               Grave            |
               Altar (Maybe Alignment) | -- alignment may not be known
               Trap             |  -- TODO: distinguish traps
               Tree             |
               Water            |  -- TODO: distinguish pools and moats
               Lava             |
               DrawbridgeClosed |
               DrawbridgeOpened |
               Cloud            |
               Corridor         |
               Air              |
               Rock             |
               Fountain         |
               IronBars         |
               Sink             |
               Unknown
               deriving(Eq, Show)

elemAt :: Level -> (Int, Int) -> Maybe Element
elemAt (Level { elements = elems }) coords = M.lookup coords elems

featureAt :: Level -> Coords -> Maybe Feature
featureAt level coords = elemAt level coords >>= feature

elemAtDefault :: Level -> (Int, Int) -> Element
elemAtDefault (Level { elements = elems }) coords =
  M.findWithDefault (initialElement weirdAppearance) coords elems

weirdAppearance :: Appearance
weirdAppearance = ("pahvilaatikko", T.defaultAttributes)

initialElement :: Appearance -> Element
initialElement appearance =
  Element { searched = 0,
            walked = 0,
            diggable = True,
            boulder = False,
            lookedLike = appearance,
            unexploredItems = False,
            items = M.empty,
            monster = Nothing,
            feature = Nothing }

setElements :: Level -> M.Map (Int, Int) Element -> Level
setElements level map = level { elements = map }

setUnexploredItems :: Element -> Bool -> Element
setUnexploredItems e b = e { unexploredItems = b }

setAppearance :: Element -> Appearance -> Element
setAppearance e a = e { lookedLike = a }

setBoulder :: Element -> Bool -> Element
setBoulder e b = e { boulder = b }

setFeature :: Element -> Maybe Feature -> Element
setFeature e f = e { feature = f }

setItems :: Element -> M.Map (Maybe Char) [Item] -> Element
setItems e items = e { items = items }

updateElement :: Level -> Coords -> (Element -> Element) -> Level
updateElement level@(Level { elements = elems }) coords changer =
  let oldElem = elemAtDefault level coords
      newElem = changer oldElem
   in setElements level (M.insert coords newElem elems)

removeMonster :: Element -> Element
removeMonster e = e { monster = Nothing }

removeBoulder :: Element -> Element
removeBoulder e = e { boulder = False }

setMonsterInstance :: Element -> Maybe MonsterInstance -> Element
setMonsterInstance e m = e { monster = m }

forbidMovementFrom :: Level -> (Int, Int) -> (Int, Int) -> Level
forbidMovementFrom level from to =
  level { forbiddenMoves = S.insert (from, to) (forbiddenMoves level) }

newLevel :: Int -> (Level, Int)
newLevel id = (Level { number = 1,
                       levelID = id,
                       elements = M.empty,
                       forbiddenMoves = S.empty,
                       endGame = False },
               id + 1)

-- for debugging
featureStr :: [Feature] -> String
featureStr [Wall] = "#"
featureStr [Floor] = "."
featureStr [OpenedDoor] = "|"
featureStr [] = "%"
featureStr x
  | length x > 1 = "!"
  | otherwise    = "?"

featureByStr :: String -> Maybe Feature
featureByStr "floor of a room" = Just Floor
featureByStr "doorway" = Just Floor
featureByStr "broken door" = Just Floor
featureByStr "open door" = Just OpenedDoor
featureByStr "closed door" = Just ClosedDoor
featureByStr "staircase up" = Just $ UpStairs Nothing
featureByStr "staircase down" = Just $ DownStairs Nothing
featureByStr "corridor" = Just Corridor
featureByStr "fountain" = Just Fountain
featureByStr "dark part of a room" = Nothing
featureByStr "ladder down" = Just $ DownLadder Nothing
featureByStr "ladder up" = Just $ UpLadder Nothing
featureByStr "opulent throne" = Just Throne
featureByStr "air" = Just Air
featureByStr "cloudy area" = Just Cloud
featureByStr "molten lava" = Just Lava
featureByStr "water" = Just Water
featureByStr "wall" = Just Wall
featureByStr "lawful altar" = Just (Altar (Just Lawful))
featureByStr "neutral altar" = Just (Altar (Just Neutral))
featureByStr "chaotic altar" = Just (Altar (Just Chaotic))
featureByStr "lawful" = Just (Altar (Just Lawful))
featureByStr "neutral" = Just (Altar (Just Neutral))
featureByStr "chaotic" = Just (Altar (Just Chaotic))
featureByStr "unaligned" = Just (Altar (Just Unaligned))
featureByStr "unaligned altar" = Just (Altar (Just Unaligned))
featureByStr "aligned altar" = Just (Altar Nothing)
featureByStr "tree" = Just Tree
featureByStr "grave" = Just Grave
featureByStr "spiked pit" = Just Trap
featureByStr "pit" = Just Trap
featureByStr "polymorph trap" = Just Trap
featureByStr "magic trap" = Just Trap
featureByStr "fire trap" = Just Trap
featureByStr "sleeping gas trap" = Just Trap
featureByStr "falling rock trap" = Just Trap
featureByStr "magic portal" = Just (Portal Nothing)
featureByStr "sink" = Just Sink
featureByStr _ = Nothing

featureByCh :: Char -> Attributes -> [Feature]
featureByCh '.' att
  | foreground att == White      = [Floor]
  | foreground att == Yellow     = [DrawbridgeOpened]
  | otherwise                    = []
featureByCh '#' att
  | foreground att == White      = [Corridor]
  | foreground att == Green      = [Tree]
  | foreground att == Cyan       = [IronBars]
  | otherwise                    = []
featureByCh '6' att
  | foreground att == White      = [Cloud]
  | otherwise                    = []
featureByCh '|' att
  | foreground att == White      = [Wall]
  | foreground att == Yellow     = [OpenedDoor]
  | otherwise                    = []
featureByCh '9' att
  | foreground att == White      = [Grave]
  | otherwise                    = []
featureByCh '-' att
  | foreground att == White      = [Wall]
  | foreground att == Yellow     = [OpenedDoor]
  | otherwise                    = []
featureByCh '^' att
  | foreground att == Magenta &&
    bold att                     = [Portal Nothing]
  | otherwise                    = [Trap]
featureByCh '\\' _ = [Throne]
featureByCh '<' att
  | foreground att == White      = [UpStairs Nothing]
  | foreground att == Yellow     = [UpLadder Nothing]
  | otherwise                    = []
featureByCh '>' att
  | foreground att == White      = [DownStairs Nothing]
  | foreground att == Yellow     = [DownLadder Nothing]
  | otherwise                    = []
featureByCh ' ' att = []
featureByCh '}' att
  | foreground att == Blue       = [Water]
  | foreground att == Red        = [Lava]
  | foreground att == White      = [Sink]
  | foreground att == Yellow     = [ClosedDoor]
  | otherwise                    = []
featureByCh '{' att
  | foreground att == Blue       = [Fountain]
  | otherwise                    = []
featureByCh '_' att
  | foreground att == White      = [Altar Nothing]
  | otherwise                    = []
featureByCh '"' att
  | foreground att == White      = [Trap]
  | otherwise                    = []
featureByCh _ _ = []

passableFeature :: Feature -> Bool
passableFeature Throne = True
passableFeature Floor = True
passableFeature UnknownFloor = True
passableFeature (DownStairs _) = True
passableFeature (UpStairs _) = True
passableFeature (UpLadder _) = True
passableFeature (DownLadder _) = True
passableFeature Grave = True
passableFeature (Altar _) = True
passableFeature Trap = True
passableFeature OpenedDoor = True
passableFeature DrawbridgeOpened = True
passableFeature Cloud = True
passableFeature Air = True
passableFeature Fountain = True
passableFeature Sink = True
passableFeature Corridor = True
passableFeature _ = False

isDungeonFeature :: String -> Bool
isDungeonFeature "0" = False
isDungeonFeature _   = True

levelCoordinates :: [(Int, Int)]
levelCoordinates = [(x, y) | x <- [1..80], y <- [2..22]]

levelCoordinatesExcept :: (Int, Int) -> [(Int, Int)]
levelCoordinatesExcept coords = levelCoordinates \\ [coords]

explorablePositions :: Level -> [(Int, Int)]
explorablePositions level =
  foldl accumFun [] coords
  where
    accumFun accum coord =
      let elem = elemAt level coord
          feat = feature $ fromJust elem
       in if any (\neighbourcoord ->
                    let elem = elemAt level neighbourcoord
                        feat = feature $ fromJust elem
                     in elem == Nothing || feat == Nothing)
                 (neighbourCoordinates coord) ||
             (elem /= Nothing && feat == Just UnknownFloor)
        then coord:accum
        else accum
    coords = [(x, y) | x <- [2..79], y <- [3..21]]

explorableReachablePositions :: Level -> (Int, Int) -> [(Int, Int)]
explorableReachablePositions l c@(x, y) =
  (reachablePositions l c `intersect` explorablePositions l) \\ [(x,y)]

reachablePositions :: Level -> (Int, Int) -> [(Int, Int)]
reachablePositions level (x, y) =
  accum S.empty (S.singleton (x, y))
  where
    accum reached check =
      if S.size check > 0
        then let minitem = S.findMin check
              in accum (S.insert minitem reached)
                       (S.delete minitem
                         (foldl (\set neighbour ->
                                   if not (S.member neighbour reached) &&
                                      canPassFrom level minitem neighbour
                                     then S.insert neighbour set
                                     else set)
                                check (neighbourCoordinates minitem)))
        else S.elems reached

canPassFrom :: Level ->
               (Int, Int) -> (Int, Int) -> Bool
canPassFrom level (x1, y1) (x2, y2)
  | x2 < 1 || y2 < 2 || y2 > 22 || x2 > 80 = False

  | elem2 == Nothing = False
  | feature jelem2 == Nothing = False
  | (passableFeature . fromJust . feature) jelem2 == False = False

  | (join (fmap feature elem1) == Just OpenedDoor ||
     join (fmap feature elem2) == Just OpenedDoor) =
    if (x1 == x2 || y1 == y2) then True else False

  | otherwise = True

  where
    elem1 = elemAt level (x1, y1) :: Maybe Element
    elem2 = elemAt level (x2, y2) :: Maybe Element
    jelem2 = fromJust elem2

neighbourCoordinates :: (Int, Int) -> [(Int, Int)]
neighbourCoordinates (x, y) =
  [(x-1, y),
   (x+1, y),
   (x-1, y-1),
   (x+1, y-1),
   (x-1, y+1),
   (x+1, y+1),
   (x, y-1),
   (x, y+1)]

sortByDistance :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
sortByDistance (x, y) coords =
  sortBy (\coords1 coords2 -> compare (dist coords1) (dist coords2)) coords
  where
    dist (x2, y2) = max (abs (x2-x)) (abs (y2-y))

boulderAt :: Level -> Coords -> Bool
boulderAt l coords =
  case elemAt l coords >>= (Just . boulder) of
    Just True -> True
    _ -> False

findPathTo :: Level -> Coords -> Coords -> Maybe [Coords]
findPathTo level =
  AStar.findPathTo (\coords ->
                     (filter (\target ->
                               (not $ boulderAt level target) &&
                               canPassFrom level coords target)
                             $ neighbourCoordinates coords))

isNextTo :: Coords -> Coords -> Bool
isNextTo (x1,y1) (x2,y2) = abs (x2-x1) <= 1 &&
                           abs (y2-y1) <= 1

findClosedDoors :: Level -> [Coords]
findClosedDoors l =
  filter (\coords -> (elemAt l coords >>= feature) == Just ClosedDoor)
         levelCoordinates

findDownstairs :: Level -> [Coords]
findDownstairs l =
  filter (\coords -> case elemAt l coords >>= feature of
                       Just (DownLadder _) -> True
                       Just (DownStairs _) -> True
                       _ -> False) levelCoordinates

maybeMarkAsOpenDoor :: Level -> T.Terminal -> Coords -> Level
maybeMarkAsOpenDoor level term coords
  | featureAt level coords /= Just ClosedDoor = level
  | featureByCh char attributes /= []   = level
  | otherwise                           =
       updateElement level coords
                     (\elem -> setFeature elem (Just OpenedDoor))
  where
    char = head $ T.strAt coords term
    attributes = T.attributesAt coords term

setStairsLevel :: Element -> LevelID -> Element
setStairsLevel el id = setStairsLevel2 (feature el) id
  where
  setStairsLevel2 :: Maybe Feature -> LevelID -> Element
  setStairsLevel2 (Just (DownLadder _)) id =
    setFeature el (Just $ DownLadder $ Just id)
  setStairsLevel2 (Just (DownStairs _)) id =
    setFeature el (Just $ DownStairs $ Just id)


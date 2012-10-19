module NetHack.Data.Level
  (initialElement,
   Level(),
   newLevel,
   setBoulders,
   setMonsters,
   feature,
   elements,
   setElements,
   lookedLike,
   Element(),
   featureByStr,
   featureByCh,
   isDungeonFeature,
   levelCoordinates)
  where

import NetHack.Data.MonsterInstance(MonsterInstance)
import Terminal.Data(Attributes, Color(..), foreground, bold, defaultAttributes)

import Data.Array(Array, array)

import NetHack.Data.Alignment
import NetHack.Data.Item

data Level = Level { number   :: Int,
                     levelId  :: Int,
                     elements :: Array (Int, Int) Element,
                     endGame  :: Bool,
                     boulders :: [(Int, Int)],
                     items    :: [((Int, Int), Item)],
                     monsters :: [((Int, Int), MonsterInstance)] }
                     deriving(Show)

data Element = Element { searched :: Int,
                         walked   :: Int,
                         diggable :: Bool,
                         lookedLike :: (String, Attributes),
                         feature  :: [Feature] }
                       deriving(Show)

type LevelID = Int

data Feature = DownStairs (Maybe LevelID) |
               UpStairs   (Maybe LevelID) |
               DownLadder (Maybe LevelID) |
               UpLadder   (Maybe LevelID) |
               Portal     (Maybe LevelID) |
               Throne           |
               Floor            |
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

setElements :: Level -> Array (Int, Int) Element -> Level
setElements lev arr = lev { elements = arr }

setBoulders :: Level -> [(Int, Int)] -> Level
setBoulders lev boulders = lev { boulders = boulders }

setMonsters :: Level -> [((Int, Int), MonsterInstance)] -> Level
setMonsters lev monsters = lev { monsters = monsters }

initialElement :: Element
initialElement = Element { searched = 0,
                           walked = 0,
                           diggable = True,
                           lookedLike = ("karamelli", defaultAttributes),
                           feature = [] }

newLevel :: Int -> (Level, Int)
newLevel id = (Level { number = 1,
                       levelId = id,
                       elements = array ((1, 2) :: (Int, Int),
                                         (80, 22) :: (Int, Int))
                                        [((x,y), initialElement) |
                                           x <- [1..80],
                                           y <- [2..22]],
                       endGame = False,
                       boulders = [],
                       items = [],
                       monsters = [] },
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
featureByCh '+' _ = []    -- Could be a spellbook
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

isDungeonFeature :: String -> Bool
isDungeonFeature "0" = False
isDungeonFeature _   = True

levelCoordinates :: [(Int, Int)]
levelCoordinates = [(x, y) | x <- [1..80], y <- [2..22]]


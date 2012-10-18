module NetHack.LevelLogic(deduceFeatureByCh, deduceFeatureByStr)
  where

import NetHack.Monad.NHAction
import NetHack.Monster
import NetHack.Item
import NetHack.Alignment
import Terminal as T


deduceFeatureByStr :: String -> Maybe Feature
deduceFeatureByStr "floor of a room" = Just Floor
deduceFeatureByStr "doorway" = Just Floor
deduceFeatureByStr "broken door" = Just Floor
deduceFeatureByStr "open door" = Just OpenedDoor
deduceFeatureByStr "closed door" = Just ClosedDoor
deduceFeatureByStr "staircase up" = Just $ UpStairs Nothing
deduceFeatureByStr "staircase down" = Just $ DownStairs Nothing
deduceFeatureByStr "corridor" = Just Corridor
deduceFeatureByStr "fountain" = Just Fountain
deduceFeatureByStr "dark part of a room" = Nothing
deduceFeatureByStr "ladder down" = Just $ DownLadder Nothing
deduceFeatureByStr "ladder up" = Just $ UpLadder Nothing
deduceFeatureByStr "opulent throne" = Just Throne
deduceFeatureByStr "air" = Just Air
deduceFeatureByStr "cloudy area" = Just Cloud
deduceFeatureByStr "molten lava" = Just Lava
deduceFeatureByStr "water" = Just Water
deduceFeatureByStr "wall" = Just Wall
deduceFeatureByStr "lawful altar" = Just (Altar (Just Lawful))
deduceFeatureByStr "neutral altar" = Just (Altar (Just Neutral))
deduceFeatureByStr "chaotic altar" = Just (Altar (Just Chaotic))
deduceFeatureByStr "lawful" = Just (Altar (Just Lawful))
deduceFeatureByStr "neutral" = Just (Altar (Just Neutral))
deduceFeatureByStr "chaotic" = Just (Altar (Just Chaotic))
deduceFeatureByStr "unaligned" = Just (Altar (Just Unaligned))
deduceFeatureByStr "unaligned altar" = Just (Altar (Just Unaligned))
deduceFeatureByStr "aligned altar" = Just (Altar Nothing)
deduceFeatureByStr "tree" = Just Tree
deduceFeatureByStr "grave" = Just Grave
deduceFeatureByStr "spiked pit" = Just Trap
deduceFeatureByStr "pit" = Just Trap
deduceFeatureByStr "polymorph trap" = Just Trap
deduceFeatureByStr "magic trap" = Just Trap
deduceFeatureByStr "fire trap" = Just Trap
deduceFeatureByStr "sleeping gas trap" = Just Trap
deduceFeatureByStr "falling rock trap" = Just Trap
deduceFeatureByStr "magic portal" = Just (Portal Nothing)
deduceFeatureByStr _ = Nothing

deduceFeatureByCh :: Char -> Attributes -> [Feature]
deduceFeatureByCh '.' att
  | foreground att == Default ||
    foreground att == White      = [Floor]
  | foreground att == Yellow     = [DrawbridgeOpened]
  | otherwise                    = []
deduceFeatureByCh '#' att
  | foreground att == Default ||
    foreground att == White      = [Corridor]
  | foreground att == Green      = [Tree]
  | foreground att == Cyan       = [IronBars]
  | otherwise                    = []
deduceFeatureByCh '6' att
  | foreground att == Default ||
    foreground att == White      = [Cloud]
  | otherwise                    = []
deduceFeatureByCh '|' att
  | foreground att == Default ||
    foreground att == White      = [Wall]
  | foreground att == Yellow     = [OpenedDoor]
  | otherwise                    = []
deduceFeatureByCh '9' att
  | foreground att == Default ||
    foreground att == White      = [Grave]
  | otherwise                    = []
deduceFeatureByCh '-' att
  | foreground att == Default ||
    foreground att == White      = [Wall]
  | foreground att == Yellow     = [OpenedDoor]
deduceFeatureByCh '+' _ = []    -- Could be a spellbook
deduceFeatureByCh '^' att
  | foreground att == Magenta &&
    bold att       == True       = [Portal Nothing]
  | otherwise                    = [Trap]
deduceFeatureByCh '\\' _ = [Throne]
deduceFeatureByCh '<' att
  | foreground att == Default ||
    foreground att == White      = [UpStairs Nothing]
  | foreground att == Yellow     = [UpLadder Nothing]
  | otherwise                    = []
deduceFeatureByCh '>' att
  | foreground att == Default ||
    foreground att == White      = [DownStairs Nothing]
  | foreground att == Yellow     = [DownLadder Nothing]
  | otherwise                    = []
deduceFeatureByCh ' ' att = []
deduceFeatureByCh '}' att
  | foreground att == Blue       = [Water]
  | foreground att == Red        = [Lava]
  | otherwise                    = []
deduceFeatureByCh '{' att
  | foreground att == Blue       = [Fountain]
  | otherwise                    = []
deduceFeatureByCh '_' att
  | foreground att == Default ||
    foreground att == White      = [Altar Nothing]
  | otherwise                    = []
deduceFeatureByCh '"' att
  | foreground att == White      = [Trap]
  | otherwise                    = []
deduceFeatureByCh _ _ = []


-- | This module defines datastructures for level transitions.
--
-- Level transitions record how to link levels together when a level change
-- happens. The bot creates a level transition when it expects the level to
-- change and then uses the transition to link the levels together.
module NetHack.Data.LevelTransition
  (LevelTransition(),
   linkLevels,
   goingDownstairsTransition)
  where

import NetHack.Data.Level hiding (DownStairs)

data LevelTransition = DownStairs Int Coords

-- | This function makes a transition that expects stairs or ladder taken
-- down. It takes two parameters: one that tells from which level stairs
-- were taken and the coordinates where the stairs are on that level.
--
goingDownstairsTransition :: Int -> Coords -> LevelTransition
goingDownstairsTransition = DownStairs

-- | 'linkLevels' modifies a level to link it to the new level according to
-- a transition. Returns the modified old level.
linkLevels :: LevelTransition -> Level -> Level -> Level
linkLevels (DownStairs _ coords) oldLevel newLevel =
  updateElement oldLevel coords modifier
  where
    modifier oldElem =
      setStairsLevel oldElem (levelID newLevel)


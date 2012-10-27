module NetHack.Control.LevelTransition
  (applyTransition)
  where

import NetHack.Monad.NHAction
import NetHack.Data.LevelTransition
import NetHack.Data.Level
import Data.Maybe(fromJust)

applyTransition :: NHAction ()
applyTransition = do
  id <- nextRunningIDM
  oldLev <- getLevelM
  let (lev, _) = newLevel id
  transitionMaybe <- getLevelTransitionM
  let transition = fromJust transitionMaybe
      modifiedOldLev = linkLevels transition oldLev lev
  putLevelM modifiedOldLev
  putCurrentLevelM lev


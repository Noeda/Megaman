module NetHack.Control.LevelTransition
  (applyTransition)
  where

import NetHack.Monad.NHAction
import NetHack.Data.LevelTransition
import NetHack.Data.Level

import Control.Monad.IO.Class

applyTransition :: NHAction ()
applyTransition = do
  id <- nextRunningIDM
  oldLev <- getLevelM
  let (lev, _) = newLevel id
  putLevelM lev
  transitionMaybe <- getLevelTransitionM
  let Just transition = transitionMaybe
      modifiedOldLev = linkLevels transition oldLev lev
  putLevelM modifiedOldLev
  putCurrentLevelM lev
  resetLevelTransitionM


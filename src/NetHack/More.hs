module NetHack.More where

import NetHack.Monad.NHAction
import NetHack.Messages
import NetHack.Screens
import Control.Monad(foldM)

harmlessMores :: [NHAction Bool]
harmlessMores = [itIsWrittenInTheBook, welcomeBackToNetHack]

skipMores :: NHAction ()
skipMores = do
  pleaseRepeat <-
    foldM (\result ac -> do test <- ac
                            if test then answer ' ' else return ()
                            if result then return True else ac)
          False harmlessMores
  if pleaseRepeat then skipMores else return ()


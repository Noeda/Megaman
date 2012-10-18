module NetHack.More where

import NetHack.Monad.NHAction
import NetHack.Messages
import NetHack.Screens
import Control.Monad(foldM, when)

harmlessMores :: [NHAction Bool]
harmlessMores = [itIsWrittenInTheBook, welcomeBackToNetHack]

skipMores :: NHAction ()
skipMores = do
  pleaseRepeat <-
    foldM (\result ac -> do test <- ac
                            when test $ answer ' '
                            if result then return True else ac)
          False harmlessMores
  when pleaseRepeat skipMores


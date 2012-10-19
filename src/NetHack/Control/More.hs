module NetHack.Control.More
  (skipMores)
  where

import Data.Foldable
import Control.Monad(when)
import NetHack.Monad.NHAction
import NetHack.Control.Screen

harmlessMores :: [NHAction Bool]
harmlessMores = [itIsWrittenInTheBook, welcomeBackToNetHack]

skipMores :: NHAction ()
skipMores = do
  pleaseRepeat <-
    foldlM (\result ac -> do test <- ac
                             when test $ answer ' '
                             if result then return True else ac)
           False harmlessMores
  when pleaseRepeat skipMores


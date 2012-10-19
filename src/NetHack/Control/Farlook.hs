module NetHack.Control.Farlook
  (farLook)
  where

import Control.Monad(when)
import NetHack.Monad.NHAction
import NetHack.Control.Screen
import NetHack.Data.Messages(trim)
import qualified Terminal.Data as T
import qualified Terminal.Terminal as T

farLook :: (Int, Int) -> NHAction String
farLook (x, y) = do
  t <- getTerminalM
  let (cx, cy) = (T.cursorX t, T.cursorY t)
  let str      = ";" ++
                 replicate (x - cx) 'l' ++
                 replicate (cx - x) 'h' ++
                 replicate (y - cy) 'j' ++
                 replicate (cy - y) 'k' ++ "."
  answer str
  str <- farLookResult
  shouldIskipMore <- morePrompt
  when shouldIskipMore $ answer ' '
  return str

farLookResult :: NHAction String
farLookResult = do
  t <- getTerminalM
  case T.captureString "\\((.+)\\)" (1, 1) (80, 2) t of
    Just r  -> return r
    Nothing ->
      case T.captureString " an? (.+) *$" (1, 1) (80, 1) t of
        Just r -> return $ trim r
        Nothing -> return ""



module NetHack.LevelPlumbing where

import NetHack.State
import NetHack.LogicPlumbing
import qualified Terminal as T

isCurrentLevel :: Int -> BAction
isCurrentLevel level =
  boolAction sinkAction sinkAction
    (\ns -> do
      result <- return $ captureLevelFromScreen (terminal ns)
      case result of
        Nothing -> return (ns, (levelId . currentLevel) ns == level) -- rely on memory
        Just x  -> return (ns, x == level))

captureLevelFromScreen :: T.Terminal -> Maybe Int
captureLevelFromScreen = T.captureInteger "Dlvl:([0-9]+)" (1, 23) (80, 23)


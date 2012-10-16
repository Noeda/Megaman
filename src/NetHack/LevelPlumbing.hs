module NetHack.LevelPlumbing where

import NetHack.State
import NetHack.LevelLogic
import NetHack.LogicPlumbing
import qualified Terminal as T

import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST

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

type STElementArray s = ST s (STArray s (Int, Int) Element)

updateCurrentLevel :: NAction
updateCurrentLevel = modAction (\ns -> return $
  let newarr = runST $ do
        marr <- (thaw $ (elements . currentLevel) ns) :: STElementArray s
        mapM_ (\coords -> do arrelem <- readArray marr coords
                             writeArray marr coords
                               (updateElem ns arrelem coords))
              [(x, y) | x <- [1..80], y <- [2..22]]
        freeze marr
   in ns { currentLevel = (currentLevel ns) { elements = newarr } })

  where
    tElemAt ns (x, y) = (T.elements (terminal ns)) ! (x, y)
    updateElem ns elem (x, y) =
      elem { feature = deductions }
      where
        tElem = tElemAt ns (x, y)
        deductions = deduceFeatureByCh ((head . T.string) tElem)
                                       (T.attrs tElem)




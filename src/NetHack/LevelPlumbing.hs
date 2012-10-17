module NetHack.LevelPlumbing(updateCurrentLevel) where

import NetHack.LevelLogic
import NetHack.LogicPlumbing
import qualified Terminal as T

import NetHack.Monad.NHAction
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST

captureLevelFromScreen :: T.Terminal -> Maybe Int
captureLevelFromScreen = T.captureInteger "Dlvl:([0-9]+)" (1, 23) (80, 23)

type STElementArray s = ST s (STArray s (Int, Int) Element)

updateCurrentLevel :: NHAction ()
updateCurrentLevel = do
  ns <- get
  put $ ns { currentLevel = (currentLevel ns) { elements = newarr ns } }
  where newarr ns = runST $ do
          marr <- (thaw $ (elements . currentLevel) ns) :: STElementArray s
          mapM_ (\coords -> do arrelem <- readArray marr coords
                               writeArray marr coords
                                 (updateElem ns arrelem coords))
                [(x, y) | x <- [1..80], y <- [2..22]]
          freeze marr
        tElemAt ns (x, y) = (T.elements (terminal ns)) ! (x, y)
        updateElem ns elem (x, y) =
          if looking /= lookedLike elem
            then elem { feature = deductions, lookedLike = looking }
            else elem
          where
            tElem = tElemAt ns (x, y)
            looking = (T.string tElem, T.attrs tElem)
            oldfeatures = feature elem
            deductions = deduceFeatureByCh ((head . T.string) tElem)
                                           (T.attrs tElem)

{-
fineTuning :: NAction
fineTuning = modActionSpawn (checker 1 2)
  where
    checker x y =
      (\ns -> let feats = features $ elements ! (x, y)
               in return $ if length feats > 1
                             then (ns, clarifier x y =+= nextChecker x y)
                             else (ns, nextChecker x y)
    nextChecker x y =
      modActionSpawn (\ns -> if x < 80
                       then (checker (1 + x) y)
                       else if y < 22
                         then (checker 1 (y + 1)
                         else (\ns -> return (ns, Nothing))
-}

cursorIsAt :: Int -> Int -> NHAction Bool
cursorIsAt x y = do
  t <- getTerminal
  return $ x == T.cursorX t &&
           y == T.cursorY t

{-
clarifier :: Int -> Int -> NAction
clarifier x y = farLook x y =+= updatePositionInformationFL x y


farLook :: Int -> Int -> NAction
farLook x y = answer ';'       =+=
              moveCursorTo x y =+=
              answer '.'
-}


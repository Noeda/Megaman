module NetHack.LevelPlumbing(updateCurrentLevel) where

import NetHack.Messages(trim)
import NetHack.Screens
import NetHack.LevelLogic
import NetHack.LogicPlumbing
import qualified Terminal as T

import Data.Foldable(foldlM)
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
  farLookUncertainPlaces
  where newarr ns = runST $ do
          marr <- (thaw $ (elements . currentLevel) ns) :: STElementArray s
          mapM_ (\coords -> do arrelem <- readArray marr coords
                               writeArray marr coords
                                 (updateElem ns arrelem coords))
                levelCoordinates
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

cursorIsAt :: Int -> Int -> NHAction Bool
cursorIsAt x y = do
  t <- getTerminal
  return $ x == T.cursorX t &&
           y == T.cursorY t

levelCoordinates :: [(Int, Int)]
levelCoordinates = [(x, y) | x <- [1..80], y <- [2..22]]

farLookUncertainPlaces :: NHAction ()
farLookUncertainPlaces = do
  ns <- get
  let elems = (elements . currentLevel) ns
  elems2 <- foldlM farLookStep elems levelCoordinates
  put $ ns { currentLevel = (currentLevel ns) { elements = elems2 } }

farLookStep :: Array (Int, Int) Element ->
               (Int, Int) ->
               NHAction (Array (Int, Int) Element)
farLookStep elems coords =
 if (length . feature) (elems ! coords) > 1 then do
      farlooked <- farLook coords
      case deduceFeatureByStr farlooked of
        Just feature -> return $ elems //
          [(coords, (elems ! coords) { feature = [feature] })]
        Nothing      -> return $ elems
    else return $ elems

farLook :: (Int, Int) -> NHAction String
farLook (x, y) = do
  t <- getTerminal
  let (cx, cy) = (T.cursorX t, T.cursorY t)
  let str      = ";" ++
                 (take (x - cx) $ repeat 'l') ++
                 (take (cx - x) $ repeat 'h') ++
                 (take (y - cy) $ repeat 'j') ++
                 (take (cy - y) $ repeat 'k') ++ "."
  answer str
  str <- farLookResult
  shouldIskipMore <- morePrompt
  if shouldIskipMore then answer ' ' else return ()
  return str

farLookResult :: NHAction String
farLookResult = do
  t <- getTerminal
  case T.captureString "\\((.+)\\)" (1, 1) (80, 2) t of
    Just r  -> return $ r
    Nothing ->
      case T.captureString " an? (.+) *$" (1, 1) (80, 1) t of
        Just r -> return $ trim r
        Nothing -> return ""




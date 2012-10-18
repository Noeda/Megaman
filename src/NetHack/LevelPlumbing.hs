module NetHack.LevelPlumbing(updateCurrentLevel) where

import NetHack.Messages(trim)
import NetHack.Screens
import NetHack.LevelLogic
import NetHack.LogicPlumbing
import qualified Terminal as T


import qualified Data.ByteString.Char8 as B
import qualified NetHack.Vanilla.MonsterData as MD
import Data.Foldable(foldlM)
import NetHack.Monad.NHAction
import Control.Monad(when)
import Control.Monad.ST
import Data.Array
import Data.Array.MArray
import Data.Array.ST
import NetHack.Monster

captureLevelFromScreen :: T.Terminal -> Maybe Int
captureLevelFromScreen = T.captureInteger "Dlvl:([0-9]+)" (1, 23) (80, 23)

type STElementArray s = ST s (STArray s (Int, Int) Element)

updateCurrentLevel :: NHAction ()
updateCurrentLevel = do
  updateMonsters
  updateBoulders
  updateDungeonFeatures

updateBoulders :: NHAction ()
updateBoulders = do
  ns <- get
  t <- getTerminal
  put $ ns { currentLevel = newboulders t $ currentLevel ns }
  where
    newboulders t l = l { boulders =
      foldl (\boulders coords ->
               if T.strAt coords t == "0"
                 then coords:boulders
                 else boulders) [] levelCoordinates }

updateMonsters :: NHAction ()
updateMonsters = do
  ns <- get
  t <- getTerminal
  l <- getLevel
  newmonsters <- foldNewMonsters t l
  put $ ns { currentLevel = l { monsters = newmonsters } }
  where
    foldNewMonsters t l =
      foldlM (accumulateNewMonsters t) [] levelCoordinates
    accumulateNewMonsters t monsters coords =
      case monsterByAppearance (T.strAt coords t)
                               (T.attrsAt coords t) of
        []      -> return monsters
        [x]     -> return $ (coords, MonsterInst x defaultMonsterAttrs):monsters
        more    -> do result <- farLook coords
                      let (trimmed, attrs) = monsterNameTrim result
                      case MD.monster $ B.pack trimmed of
                        Nothing -> return monsters
                        Just m  -> return $ (coords, MonsterInst m attrs):monsters

nonDungeonFeature :: NetHackState -> (Int, Int) -> Bool
nonDungeonFeature (NetHackState { terminal = t }) (x, y) =
  case T.strAt (x, y) t of
    "0" -> True
    _   -> False

updateDungeonFeatures :: NHAction ()
updateDungeonFeatures = do
  ns <- get
  put $ ns { currentLevel = (currentLevel ns) { elements = newarr ns } }
  farLookUncertainPlaces
  where newarr ns = runST $ do
          marr <- (thaw $ (elements . currentLevel) ns) :: STElementArray s
          mapM_ (\coords -> do arrelem <- readArray marr coords
                               writeArray marr coords
                                 (updateElem ns arrelem coords))
                $ filter (not . nonDungeonFeature ns) levelCoordinates
          freeze marr
        tElemAt ns (x, y) = T.elements (terminal ns) ! (x, y)
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
        Nothing      -> return elems
    else return elems

farLook :: (Int, Int) -> NHAction String
farLook (x, y) = do
  t <- getTerminal
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
  t <- getTerminal
  case T.captureString "\\((.+)\\)" (1, 1) (80, 2) t of
    Just r  -> return r
    Nothing ->
      case T.captureString " an? (.+) *$" (1, 1) (80, 1) t of
        Just r -> return $ trim r
        Nothing -> return ""




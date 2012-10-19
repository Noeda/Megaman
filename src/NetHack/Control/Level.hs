module NetHack.Control.Level(updateCurrentLevel) where

import Data.Foldable(foldlM)

import NetHack.Data.Level
import NetHack.Data.NetHackState
import NetHack.Data.MonsterInstance
import qualified NetHack.Imported.MonsterData as MD
import NetHack.Monad.NHAction
import NetHack.Control.Farlook

import Control.Monad.State
import Control.Monad.ST
import Data.Array.ST
import Data.Array((!), Array, (//))

import qualified Data.ByteString.Char8 as B

import qualified Terminal.Data as T
import qualified Terminal.Terminal as T

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
  t <- getTerminalM
  lev <- getLevelM
  putLevelM $ newboulders t lev
  where
    newboulders t l = setBoulders l $
      foldl (\boulders coords ->
               if T.strAt coords t == "0"
                 then coords:boulders
                 else boulders) [] levelCoordinates

updateMonsters :: NHAction ()
updateMonsters = do
  t <- getTerminalM
  l <- getLevelM
  newmonsters <- foldNewMonsters t l
  putLevelM $ setMonsters l newmonsters
  where
    foldNewMonsters t l =
      foldlM (accumulateNewMonsters t) [] levelCoordinates
    accumulateNewMonsters t monsters coords =
      case monsterByAppearance (T.strAt coords t)
                               (T.attributesAt coords t) of
        []      -> return monsters
        [x]     -> return $
                     (coords, freshMonsterInstance x):monsters
        more    -> do result <- farLook coords
                      let (trimmed, attrs) = monsterNameTrim result
                      case MD.monster $ B.pack trimmed of
                        Nothing -> return monsters
                        Just m  -> return $
                          (coords, newMonsterInstance m attrs):monsters

nonDungeonFeatureAt :: NetHackState -> (Int, Int) -> Bool
nonDungeonFeatureAt ns (x, y) =
  not $ isDungeonFeature (T.strAt (x, y) $ terminal ns)

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
                $ filter (not . nonDungeonFeatureAt ns) levelCoordinates
          freeze marr
        tElemAt ns (x, y) = T.elements (terminal ns) ! (x, y)
        updateElem ns elem (x, y) =
          if looking /= lookedLike elem
            then elem { feature = deductions, lookedLike = looking }
            else elem
          where
            tElem = tElemAt ns (x, y)
            looking = (T.string tElem, T.attributes tElem)
            oldfeatures = feature elem
            deductions = featureByCh ((head . T.string) tElem)
                                     (T.attributes tElem)

farLookUncertainPlaces :: NHAction ()
farLookUncertainPlaces = do
  level <- getLevelM
  let oldElems = elements level
  newElems <- foldlM farLookStep oldElems levelCoordinates
  putLevelM $ setElements level newElems

farLookStep :: Array (Int, Int) Element ->
               (Int, Int) ->
               NHAction (Array (Int, Int) Element)
farLookStep elems coords =
 if (length . feature) (elems ! coords) > 1 then do
      farlooked <- farLook coords
      case featureByStr farlooked of
        Just feature -> return $ elems //
          [(coords, (elems ! coords) { feature = [feature] })]
        Nothing      -> return elems
    else return elems


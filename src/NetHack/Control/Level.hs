module NetHack.Control.Level
  (updateCurrentLevel,
   hostileMonstersWithinRangeM)
  where

import Data.Foldable(foldlM)

import NetHack.Data.Level
import NetHack.Data.NetHackState
import NetHack.Data.Appearance
import NetHack.Data.Item
import qualified NetHack.Data.MonsterInstance as MI
import qualified NetHack.Imported.MonsterData as MD
import NetHack.Monad.NHAction
import NetHack.Control.Farlook
import NetHack.Control.Screen
import NetHack.Control.ItemListing
import NetHack.Control.LevelTransition

import Control.Monad.State
import Control.Monad.ST
import Data.Array.ST
import Data.Array((!), Array, (//))

import qualified Regex as R
import qualified Data.Map as M

import qualified Data.ByteString.Char8 as B

import qualified Terminal.Data as T
import qualified Terminal.Terminal as T

type STElementArray s = ST s (STArray s (Int, Int) Element)

data ElementCandidate = Boulder |
                        Monster MD.Monster |
                        MonsterInstance MI.MonsterInstance |
                        DungeonFeature Feature
                        deriving(Show,Eq)

boulderCandidates :: Appearance -> [ElementCandidate]
boulderCandidates ("0", _) = [Boulder]
boulderCandidates _        = []

featureCandidates :: Coords -> Coords -> Appearance -> [ElementCandidate]
featureCandidates (x1, y1) (x2, y2) ([x], attributes) =
  -- Empty space next to the player is stone! I hope. Won't hold if the
  -- player is blind.
  if abs (x1-x2) <= 1 && abs (y1-y2) <= 1 && x == ' '
    then [DungeonFeature Rock]
    else map DungeonFeature $ featureByCh x attributes
featureCandidates _ _ _ = []

monsterCandidates :: Appearance -> [ElementCandidate]
monsterCandidates (str, attributes) =
  map Monster $ MI.monsterByAppearance (str, noInversion attributes)

farLookFilter :: String -> ElementCandidate -> [ElementCandidate]
farLookFilter "boulder" Boulder = [Boulder]
farLookFilter _ Boulder = []
farLookFilter str d@(DungeonFeature oldFeature) =
  case featureByStr str of
    Nothing -> []
    Just f  -> if oldFeature == f then [d] else []

farLookFilter str m@(Monster mdata) =
  let (name, mattributes) = MI.monsterNameTrim str
   in case MI.monsterByName name of
        Nothing -> []
        Just m  -> if m == mdata
                   then [MonsterInstance $ MI.newMonsterInstance m mattributes]
                   else []

maybeSetFloor :: Element -> Element
maybeSetFloor el =
  if feature el == Nothing
    then setFeature el (Just UnknownFloor)
    else el

updateWithCandidate :: Element -> ElementCandidate -> Element
updateWithCandidate element candidate =
  let newElem = updateWithCandidate2 element candidate
   -- Assume the dungeon feature is a floor if we have no better knowledge.
   in maybeSetFloor newElem
  where
  updateWithCandidate2 element (Boulder) =
    removeMonster $ setBoulder element True
  updateWithCandidate2 element (DungeonFeature f) =
    setItems (removeBoulder . removeMonster $ setFeature element (Just f))
      M.empty
  updateWithCandidate2 element (Monster m) =
    removeBoulder $ setMonsterInstance element
       (Just $ MI.newMonsterInstance m MI.defaultMonsterAttributes)
  updateWithCandidate2 element (MonsterInstance mi) =
    removeBoulder $ setMonsterInstance element (Just mi)

noInversion :: T.Attributes -> T.Attributes
noInversion attrs = T.setInverse attrs False

solidRockify :: NHAction ()
solidRockify = do
  l <- getLevelM
  t <- getTerminalM
  playercoords <- getCoordsM
  let neighbourcoords = neighbourCoordinates playercoords
  mapM_ (\coord -> if T.strAt coord t == " "
                      then do elem <- getElementM coord
                              putElementM (setFeature elem (Just Rock)) coord
                      else return ()) neighbourcoords

updateCurrentLevel :: NHAction ()
updateCurrentLevel = do
  handleDungeonLevelTransition
  -- Set solid rock next to player
  solidRockify
  l <- getLevelM
  t <- getTerminalM
  playercoords <- getCoordsM
  newElements <- foldM (updateLevelElement l t playercoords) (elements l) $
                   levelCoordinatesExcept (T.coords t)
  putLevelM $ setElements l newElements
  l <- getLevelM
  let f = (fmap feature) . (elemAt l) $ T.coords t
  when (f == Nothing || f == Just (Just UnknownFloor)) $ do
    lookDownUpdate
  where
    updateLevelElement level terminal playercoords elements coords =
      let appearance =
            (T.strAt coords terminal, T.attributesAt coords terminal)
          oldAppearance = lookedLike elem
          str = fst appearance
          elem = elemAtDefault level coords
          updateElem e = M.insert coords (setAppearance e appearance) elements
       in if appearance /= oldAppearance
     then let candidates = concat [monsterCandidates appearance,
                                   featureCandidates playercoords coords
                                                     appearance,
                                   boulderCandidates appearance]
           in do newCandidates <- if length candidates > 1
                   then do farlooked <- farLook coords
                           return $ concatMap
                                      (farLookFilter farlooked) candidates
                   else return $ candidates
                 case length newCandidates of
                   0 -> return $ if couldHaveItems appearance
                                   then updateElem $ maybeSetFloor elem
                                   else updateElem elem
                   1 -> return $ updateElem
                     (updateWithCandidate elem $ head newCandidates)
                   _ -> if couldHaveItems appearance
                          then return $
                               updateElem $ maybeSetFloor $
                                            setUnexploredItems elem True
                          else error $ "One of the squares on the level is " ++
                                     "confusing me. It looks like (" ++
                                     str ++ ") and the candidates are: " ++
                                     show newCandidates
     else return elements

while :: Functor m => Monad m => m Bool -> m a -> m ()
while test action = do
  result <- test
  when result $ do void action
                   while test action

lookDownUpdate :: NHAction ()
lookDownUpdate = do
  t <- getTerminalM
  let coords = T.coords t
  answer ':'
  t <- getTerminalM
  l <- getLevelM
  let elems = elements l
      oldElem = M.findWithDefault (initialElement weirdAppearance)
                   coords elems
      firstLine = T.lineAt 1 t
      featureNames = R.match "There is (a |an )? (.+) here\\."
                             firstLine :: [String]

  case length featureNames of
    1 -> do putElementM (setFeature oldElem (featureByStr $ featureNames !! 2))
                        coords
    0 -> do putElementM (setFeature oldElem (Just Floor)) coords
    _ -> return ()

  oldElem <- getElementM coords

  -- Single item
  case R.match "You see here (.+)\\." firstLine :: [String] of
    [] -> return ()
    [item] -> putElementM
                (if feature oldElem == Just UnknownFloor
                   then setFeature itemizedElem (Just Floor)
                   else itemizedElem) coords
              where
                itemizedElem = setItems oldElem $
                                 M.singleton Nothing $
                                 [canonicalizeItemName item]

  oldElem <- getElementM coords

  case R.match "You see no objects here." firstLine :: [String] of
    [] -> return ()
    [_] -> putElementM (setFeature oldElem (Just Floor)) coords

  let shouldExamineItems = T.isSomewhereOnScreen "Things that are here: " t
  while morePrompt $ answer ' '
  when shouldExamineItems $ examineItemsOnFloor coords

-- assumes there is more than one item on the floor.
examineItemsOnFloor :: (Int, Int) -> NHAction ()
examineItemsOnFloor coords = do
  answer ','
  oldElem <- getElementM coords
  items <- itemsOnScreen (\_ -> False)
  (flip putElementM $ coords) $ setItems oldElem (M.mapKeys Just items)

handleDungeonLevelTransition :: NHAction ()
handleDungeonLevelTransition = do
  t <- getTerminalM
  ns <- get

  let oldLevelNumber = dungeonLevel ns
  oldLevel <- getLevelM

  put $ updateDungeonLevel ns t

  ns <- get

  let newLevelNumber = dungeonLevel ns
  when (oldLevelNumber /= newLevelNumber) applyTransition

hostileMonstersWithinRangeM :: Int -> NHAction [(Coords, MI.MonsterInstance)]
hostileMonstersWithinRangeM range = do
  coords <- getCoordsM
  hostiles <- hostileMonstersM
  return $ filter (\(monstercoords, _) ->
                    distance coords monstercoords <= range) hostiles

hostileMonstersM :: NHAction [(Coords, MI.MonsterInstance)]
hostileMonstersM = do
  level <- getLevelM
  let hostiles = hostileOrUnknownMonsters level
      notDefinitelyHostiles = filter (\(_, hostile) ->
                                       MI.isHostile hostile == Nothing)
                                     hostiles

  -- farlook those that may not actually be hostile
  if notDefinitelyHostiles /= []
    then mapM_ (\coords -> do result <- farLook coords
                              level <- getLevelM
                              let (name, attributes) = MI.monsterNameTrim name
                                  (Just oldElem) = elemAt level coords
                              putElementM (setMonsterAttrs oldElem attributes)
                                          coords)
               (map fst notDefinitelyHostiles)
         >> hostileMonstersM
    else return hostiles
  where
  setMonsterAttrs :: Element -> MI.MonsterAttributes -> Element
  setMonsterAttrs e attrs =
    let (Just mon) = monster e
        newMon = MI.setAttributes mon attrs
     in setMonsterInstance e (Just newMon)


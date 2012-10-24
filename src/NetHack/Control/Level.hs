module NetHack.Control.Level(updateCurrentLevel) where

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

import Control.Monad.State
import Control.Monad.ST
import Data.Array.ST
import Data.Array((!), Array, (//))

import qualified Regex as R
import qualified Data.Map as M

import qualified Data.ByteString.Char8 as B

import qualified Terminal.Data as T
import qualified Terminal.Terminal as T

captureLevelFromScreen :: T.Terminal -> Maybe Int
captureLevelFromScreen = T.captureInteger "Dlvl:([0-9]+)" (1, 23) (80, 23)

type STElementArray s = ST s (STArray s (Int, Int) Element)

data ElementCandidate = Boulder |
                        Monster MD.Monster |
                        MonsterInstance MI.MonsterInstance |
                        DungeonFeature Feature
                        deriving(Show,Eq)

boulderCandidates :: Appearance -> [ElementCandidate]
boulderCandidates ("0", _) = [Boulder]
boulderCandidates _        = []

featureCandidates :: Appearance -> [ElementCandidate]
featureCandidates ([x], attributes) =
  map DungeonFeature $ featureByCh x attributes
featureCandidates _ = []

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
    then setFeature el (Just Floor)
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

updateCurrentLevel :: NHAction ()
updateCurrentLevel = do
  l <- getLevelM
  t <- getTerminalM
  newElements <- foldM (updateLevelElement l t) (elements l) $
                   levelCoordinatesExcept (T.coords t)
  putLevelM $ setElements l newElements
  l <- getLevelM
  when (((fmap feature) . (elemAt l)) (T.coords t) == Nothing) $ do
    lookDownUpdate
  where
    updateLevelElement level terminal elements coords =
      let appearance =
            (T.strAt coords terminal, T.attributesAt coords terminal)
          oldAppearance = lookedLike elem
          str = fst appearance
          elem = elemAtDefault level coords
          updateElem e = M.insert coords (setAppearance e appearance) elements
       in if appearance /= oldAppearance
     then let candidates = concat [monsterCandidates appearance,
                                   featureCandidates appearance,
                                   boulderCandidates appearance]
           in do newCandidates <- if length candidates > 1
                   then do farlooked <- farLook coords
                           return $ concatMap
                                      (farLookFilter farlooked) candidates
                   else return $ candidates
                 case length newCandidates of
                   0 -> return $ updateElem elem
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

  -- Single item
  case R.match "You see here (.+)\\." firstLine :: [String] of
    [] -> return ()
    [item] -> putElementM (setItems oldElem $
                           M.singleton Nothing $
                           [canonicalizeItemName item]) coords

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


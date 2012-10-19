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

import Control.Monad.State
import Control.Monad.ST
import Data.Array.ST
import Data.Array((!), Array, (//))

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

updateWithCandidate :: Element -> ElementCandidate -> Element
updateWithCandidate element (Boulder) = setBoulder element True
updateWithCandidate element (DungeonFeature f) = setFeature element (Just f)
updateWithCandidate element (MonsterInstance mi) =
  setMonsterInstance element (Just mi)

noInversion :: T.Attributes -> T.Attributes
noInversion attrs = T.setInverse attrs False

updateCurrentLevel :: NHAction ()
updateCurrentLevel = do
  l <- getLevelM
  t <- getTerminalM
  newElements <- foldM (updateLevelElement l t) (elements l) $
                   levelCoordinatesExcept (T.coords t)
  putLevelM $ setElements l newElements
  where
    updateLevelElement level terminal elements coords =
      let appearance =
            (T.strAt coords terminal, T.attributesAt coords terminal)
          oldAppearance = lookedLike elem
          str = fst appearance
          elem = M.findWithDefault (initialElement weirdAppearance)
                   coords elements
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
                               updateElem $ setUnexploredItems elem True
                          else error $ "One of the squares on the level is " ++
                                     "confusing me. It looks like (" ++
                                     str ++ ") and the candidates are: " ++
                                     show newCandidates
     else return elements


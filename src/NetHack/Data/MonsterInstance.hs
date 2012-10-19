module NetHack.Data.MonsterInstance
  (MonsterInstance(),
   monsterNameTrim,
   newMonsterInstance,
   freshMonsterInstance,
   monsterByAppearance)
  where

import Data.Maybe

import qualified NetHack.Imported.MonsterData as MD
import qualified Regex as R
import qualified Terminal.Data as T

data MonsterAttributes = MonsterAttributes
  { peaceful :: Maybe Bool,
    tame :: Maybe Bool } deriving(Eq, Show)

data MonsterInstance = MonsterInstance MD.Monster MonsterAttributes
                       deriving(Eq, Show)

newMonsterInstance :: MD.Monster -> MonsterAttributes -> MonsterInstance
newMonsterInstance = MonsterInstance

defaultMonsterAttributes = MonsterAttributes Nothing Nothing

mdCToTermAttributes :: MD.Color -> T.Attributes
mdCToTermAttributes MD.Black         = T.newAttributes T.Blue T.Black False False
mdCToTermAttributes MD.Red           = T.newAttributes T.Red T.Black False False
mdCToTermAttributes MD.Green         = T.newAttributes T.Green T.Black False False
mdCToTermAttributes MD.Brown         = T.newAttributes T.Yellow T.Black False False
mdCToTermAttributes MD.Blue          = T.newAttributes T.Blue T.Black False False
mdCToTermAttributes MD.Magenta       = T.newAttributes T.Magenta T.Black False False
mdCToTermAttributes MD.Cyan          = T.newAttributes T.Cyan T.Black False False
mdCToTermAttributes MD.Gray          = T.newAttributes T.White T.Black False False
mdCToTermAttributes MD.Orange        = T.newAttributes T.Red T.Black True False
mdCToTermAttributes MD.BrightGreen   = T.newAttributes T.Green T.Black True False
mdCToTermAttributes MD.Yellow        = T.newAttributes T.Yellow T.Black True False
mdCToTermAttributes MD.BrightBlue    = T.newAttributes T.Blue T.Black True False
mdCToTermAttributes MD.BrightMagenta = T.newAttributes T.Magenta T.Black True False
mdCToTermAttributes MD.BrightCyan    = T.newAttributes T.Cyan T.Black True False
mdCToTermAttributes MD.White         = T.newAttributes T.White T.Black True False

freshMonsterInstance mon =
  newMonsterInstance mon defaultMonsterAttributes

monsterByAppearance :: String -> T.Attributes -> [MD.Monster]
monsterByAppearance str attributes =
  foldl accumulateMonsters [] MD.allMonsterNames
  where
    accumulateMonsters accum mons =
      let m = fromJust $ MD.monster mons
       in if (mdCToTermAttributes . MD.moColor $ m) == attributes &&
             [MD.moSymbol m] == str
               then m:accum
               else accum

monsterNameTrim :: String -> (String, MonsterAttributes)
monsterNameTrim monsname = peacefulness monsname
  where
    peacefulness monsname =
      case R.match "^peaceful (.+)$" monsname of
        Just rest -> tameness rest True
        Nothing   -> tameness monsname False
    tameness monsname peaceful =
      case R.match "^tame (.+)$" monsname of
        Just rest -> coyoteness rest     (peaceful, True)
        Nothing   -> coyoteness monsname (peaceful, False)
    coyoteness monsname attrs =
      case R.match "^(.+) \\- .+$" monsname of
        Just rest -> nameness rest attrs
        Nothing   -> nameness monsname attrs
    nameness monsname (peaceful, tame) =
      case R.match "^(.+) called .+$" monsname of
        Just rest -> (rest,     MonsterAttributes (Just peaceful) (Just tame))
        Nothing   -> (monsname, MonsterAttributes (Just peaceful) (Just tame))



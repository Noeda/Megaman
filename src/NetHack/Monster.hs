module NetHack.Monster(monsterByAppearance, freshMonsterInst,
                       monsterNameTrim)
  where

import qualified Regex as R
import Data.Maybe
import Misc
import NetHack.Monad.NHAction
import qualified NetHack.Vanilla.MonsterData as MD
import qualified Terminal as T

mdCToTermAttrs :: MD.Color -> T.Attributes
mdCToTermAttrs MD.Black         = T.newAttributes T.Blue T.Black False False
mdCToTermAttrs MD.Red           = T.newAttributes T.Red T.Black False False
mdCToTermAttrs MD.Green         = T.newAttributes T.Green T.Black False False
mdCToTermAttrs MD.Brown         = T.newAttributes T.Yellow T.Black False False
mdCToTermAttrs MD.Blue          = T.newAttributes T.Blue T.Black False False
mdCToTermAttrs MD.Magenta       = T.newAttributes T.Magenta T.Black False False
mdCToTermAttrs MD.Cyan          = T.newAttributes T.Cyan T.Black False False
mdCToTermAttrs MD.Gray          = T.newAttributes T.White T.Black False False
mdCToTermAttrs MD.Orange        = T.newAttributes T.Red T.Black True False
mdCToTermAttrs MD.BrightGreen   = T.newAttributes T.Green T.Black True False
mdCToTermAttrs MD.Yellow        = T.newAttributes T.Yellow T.Black True False
mdCToTermAttrs MD.BrightBlue    = T.newAttributes T.Blue T.Black True False
mdCToTermAttrs MD.BrightMagenta = T.newAttributes T.Magenta T.Black True False
mdCToTermAttrs MD.BrightCyan    = T.newAttributes T.Cyan T.Black True False
mdCToTermAttrs MD.White         = T.newAttributes T.White T.Black True False

freshMonsterInst :: MD.Monster -> MonsterInst
freshMonsterInst mon = MonsterInst mon defaultMonsterAttrs

monsterByAppearance :: String -> T.Attributes -> [MD.Monster]
monsterByAppearance str attributes =
  foldl accumulateMonsters [] MD.allMonsterNames
  where
    accumulateMonsters accum mons =
      let m = fromJust $ MD.monster mons
       in if (mdCToTermAttrs . MD.moColor $ m) == attributes &&
             [MD.moSymbol m] == str
               then (m:accum)
               else accum

monsterNameTrim :: String -> (String, MonsterAttrs)
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
        Just rest -> (rest,     MonsterAttrs (Just peaceful) (Just tame))
        Nothing   -> (monsname, MonsterAttrs (Just peaceful) (Just tame))



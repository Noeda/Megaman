module NetHack.Logic(root) where

import NetHack.LevelLogic
import NetHack.LogicPlumbing
import NetHack.Combo
import NetHack.Alignment
import NetHack.Screens

root :: NAction
root = restoreSave =+=
       startCharacter (Combo Healer Gnome Female Neutral) =+=
       exploreLevel 1

-- Restores a save if it looks like there is one.
-- (currently saving the state is not implemented)
restoreSave :: NAction
restoreSave =
  ifIn restoringSave (answer ' ')

-- Starts a character if one has not been started yet.
-- If the combo is invalid or something else bad happens, it bails out.
startCharacter :: Combo -> NAction
startCharacter combo =
  ifIn shallIPick       (answer 'n')        =+=
  ifIn pickARole        (answer role1)      =+=
  ifIn pickTheRace      (answer race1)      =+=
  ifIn pickTheGender    (answer gender1)    =+=
  ifIn pickTheAlignment (answer alignment1) =+=

  skipMores                                 =+=

  ifNotIn gameScreen (bailout "I was not able to pick a character.")

  where
    role1 = roleLetter $ role combo
    race1 = raceLetter $ race combo
    gender1 = genderLetter $ gender combo
    alignment1 = alignmentLetter $ alignment combo

-- Explores the dungeon level as specified by an integer.
-- If the player is not the level, then the bot will attempt to get there
-- in some way. May bail out if it runs out of ideas to get on the level.
exploreLevel :: Int -> NAction
exploreLevel level =
  ifNotIn (isCurrentLevel level) sinkAction =+=
  bailout "I would explore now if I knew how to do it"

harmlessMores :: [BAction]
harmlessMores = [itIsWrittenInTheBook, welcomeBackToNetHack]

skipMores :: NAction
skipMores =
  repeatUntilNoAnswer $
    foldr (\bac naction -> naction =+= ifIn (bac =&&= morePrompt) space)
          sinkAction harmlessMores
    where
      space = answer ' '


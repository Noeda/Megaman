module NetHack.Logic(root) where

import Control.Monad
import NetHack.Monad.NHAction
import NetHack.LevelLogic
import NetHack.LevelPlumbing
import NetHack.LogicPlumbing
import NetHack.Combo
import NetHack.Alignment
import NetHack.Screens
import NetHack.More

root :: NHAction ()
root = waitForData >>
       restoreSave >>
       startCharacter (Combo Healer Gnome Female Neutral) >>
       exploreLevel

-- Restores a save if it looks like there is one.
-- (currently saving the state is not implemented)
restoreSave :: NHAction ()
restoreSave =
  void $ ifIn restoringSave (answer ' ')

-- Starts a character if one has not been started yet.
-- If the combo is invalid or something else bad happens, it bails out.
startCharacter :: Combo -> NHAction ()
startCharacter combo = do
  ifIn shallIPick       (answer 'n')
  ifIn pickARole        (answer role1)
  ifIn pickTheRace      (answer race1)
  ifIn pickTheGender    (answer gender1)
  ifIn pickTheAlignment (answer alignment1)

  skipMores

  void $ ifNotIn gameScreen (bailout "I was not able to pick a character.")

  where
    role1 = roleLetter $ role combo
    race1 = raceLetter $ race combo
    gender1 = genderLetter $ gender combo
    alignment1 = alignmentLetter $ alignment combo

-- Explores the dungeon level as specified by an integer.
-- If the player is not the level, then the bot will attempt to get there
-- in some way. May bail out if it runs out of ideas to get on the level.
exploreLevel :: NHAction ()
exploreLevel = do
  skipMores
  updateCurrentLevel



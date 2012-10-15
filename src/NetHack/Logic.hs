module NetHack.Logic(root) where

import NetHack.LogicPlumbing
import NetHack.Combo
import NetHack.Alignment
import NetHack.Screens

root :: NAction
root = startCharacter (Combo Healer Gnome Female Neutral)

-- Starts a character if one has not been started yet.
-- If the combo is invalid or something else bad happens, it bails out.
startCharacter :: Combo -> NAction
startCharacter combo =
  ifIn shallIPick       (answer 'n')        =+=
  ifIn pickARole        (answer role1)      =+=
  ifIn pickTheRace      (answer race1)      =+=
  ifIn pickTheGender    (answer gender1)    =+=
  ifIn pickTheAlignment (answer alignment1) =+=

  ifIn itIsWrittenInTheBook space           =+=

  ifNotIn gameScreen (bailout "I was not able to pick a character.")

  where
    role1 = roleLetter $ role combo
    race1 = raceLetter $ race combo
    gender1 = genderLetter $ gender combo
    alignment1 = alignmentLetter $ alignment combo
    space = answer ' '


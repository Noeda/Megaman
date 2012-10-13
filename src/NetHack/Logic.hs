module NetHack.Logic where

import NetHack.LogicPlumbing
import NetHack.Combo
import NetHack.Alignment
import NetHack.Screens

root :: NHStep ()
root = startCharacter (Combo Healer Gnome Female Neutral)

-- Starts a character if one has not been started yet.
-- Can die if the combo is invalid; then it doesn't leave tha character
-- screen.
startCharacter :: Combo -> NHStep ()
startCharacter combo =
  ifIn shallIPick       (answer 'n')        =+=
  ifIn pickARole        (answer role1)      =+=
  ifIn pickTheRace      (answer race1)      =+=
  ifIn pickTheGender    (answer gender1)    =+=
  ifIn pickTheAlignment (answer alignment1)

  --ifNotIn gameScreen (bailout "I was not able to pick a character.")

  where
    role1 = roleLetter $ role combo
    race1 = raceLetter $ race combo
    gender1 = genderLetter $ gender combo
    alignment1 = alignmentLetter $ alignment combo




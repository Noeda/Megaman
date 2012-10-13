module NetHack.Screens where

import NetHack.LogicPlumbing

shallIPick       = isSomewhereOnScreen "Shall I pick a character's"
pickARole        = isSomewhereOnScreen "Pick a role for your character"
pickTheRace      = isSomewhereOnScreen "Pick the race of your"
pickTheGender    = isSomewhereOnScreen "Pick the gender of your"
pickTheAlignment = isSomewhereOnScreen "Pick the alignment of your"


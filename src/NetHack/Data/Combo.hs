module NetHack.Data.Combo
  (Role(..),
   Race(..),
   Gender(..),
   Combo(..),
   roleLetter,
   raceLetter,
   genderLetter,
   alignmentLetter)
  where

import NetHack.Data.Alignment

data Role = Archeologist | Barbarian | Caveman | Healer |
            Knight | Monk | Priest | Rogue | Ranger |
            Samurai | Tourist | Valkyrie | Wizard

data Race = Human | Dwarf | Elf | Orc | Gnome

data Gender = Female | Male

data Combo = Combo { role :: Role,
                     race :: Race,
                     gender :: Gender,
                     alignment :: Alignment }

roleLetter :: Role -> Char
roleLetter Archeologist = 'a'
roleLetter Barbarian    = 'b'
roleLetter Caveman      = 'c'
roleLetter Healer       = 'h'
roleLetter Knight       = 'k'
roleLetter Monk         = 'm'
roleLetter Priest       = 'p'
roleLetter Rogue        = 'r'
roleLetter Ranger       = 'R'
roleLetter Samurai      = 's'
roleLetter Tourist      = 't'
roleLetter Valkyrie     = 'v'
roleLetter Wizard       = 'w'

raceLetter :: Race -> Char
raceLetter Human        = 'h'
raceLetter Elf          = 'e'
raceLetter Gnome        = 'g'
raceLetter Orc          = 'o'
raceLetter Dwarf        = 'd'

genderLetter :: Gender -> Char
genderLetter Male       = 'm'
genderLetter Female     = 'f'

alignmentLetter :: Alignment -> Char
alignmentLetter Lawful  = 'l'
alignmentLetter Neutral = 'n'
alignmentLetter Chaotic = 'c'


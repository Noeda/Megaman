module Terminal.Data
  (newAttributes,
   attributes,
   Terminal(),
   Attributes(),
   Color(..),
   width, height,
   defaultAttributes,
   defaultElement,
   foreground, background,
   bold, inverse,
   setInverse,
   TerminalArray,
   STTerminalArray,
   string,
   elements,
   strAt,
   attributesAt,
   cursorX,
   cursorY,
   coords)
  where

import Terminal.Internal
import Data.Array((!), bounds)

newAttributes :: Color -> Color -> Bool -> Bool -> Attributes
newAttributes = Attributes

cursorX :: Terminal -> Int
cursorX = cx

cursorY :: Terminal -> Int
cursorY = cy

coords :: Terminal -> (Int, Int)
coords t = (cursorX t, cursorY t)

defaultAttributes :: Attributes
defaultAttributes = Attributes White Black False False

defaultElement :: Element
defaultElement = Element " " defaultAttributes Independent

strAt :: (Int, Int) -> Terminal -> String
strAt (x, y) t = string $ elemAt (x, y) t

attributesAt :: (Int, Int) -> Terminal -> Attributes
attributesAt (x, y) t = attributes $ elemAt (x, y) t

elemAt :: (Int, Int) -> Terminal -> Element
elemAt (x, y) (Terminal { elements = elems }) = elems ! (x,y)

width :: Terminal -> Int
width Terminal { elements = arr } = fst $ snd $ bounds arr

height :: Terminal -> Int
height Terminal { elements = arr } = snd $ snd $ bounds arr

setInverse :: Attributes -> Bool -> Attributes
setInverse attrs b = attrs { inverse = b }


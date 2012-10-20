module Terminal.Internal
  (Terminal(..),
   Attributes(..),
   Color(..),
   Element(..),
   Partiality(..),
   TerminalArray(..),
   STTerminalArray(..))
  where

import Control.Monad.ST(ST)
import Data.Array.ST(STArray)

import Data.Array(Array, array, (!))

type TerminalArray = Array (Int, Int) Element
type STTerminalArray s = ST s (STArray s (Int, Int) Element)

data Terminal = Terminal { elements :: TerminalArray,
                           cx :: Int,
                           cy :: Int,
                           currentAttributes :: Attributes,
                           consumer :: Terminal -> Char -> Terminal }

data Attributes = Attributes { foreground :: Color,
                               background :: Color,
                               bold :: Bool,
                               inverse :: Bool }
                  deriving(Eq, Show)

data Element = Element { string :: String,
                         attributes :: Attributes,
                         partOf :: Partiality }


-- Some elements may be written over by some other elements
-- For example, full width CJK characters can take two elements in the
-- terminal. One element encodes the character and the other elements have
-- PartOf partiality element in them, indicating they are part of that
-- other element.
-- PartOf coordinates are relative. This allows the elements to be moved
-- around without coordinates becoming invalid.
data Partiality = Independent | PartOf Int Int

data Color = Black | Red | Green | Blue | Magenta | Yellow | Cyan | White
             deriving(Eq, Show)


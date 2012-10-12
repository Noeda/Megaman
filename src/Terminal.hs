module Terminal(Terminal, emptyTerminal, handleChar, printOut) where

import Data.Char
import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Maybe

type TerminalArray = Array (Int, Int) Elem
type STTerminalArray s = ST s (STArray s (Int, Int) Elem)

data Terminal = Terminal { elements :: TerminalArray,
                           cx :: Int,
                           cy :: Int,
                           attributes :: Attributes,
                           consumer :: Terminal -> Char -> Terminal }

data Attributes = Attributes { foreground :: Color,
                               background :: Color,
                               bold :: Bool,
                               inverse :: Bool }

-- Some elements may be written over by some other elements
-- For example, full width CJK characters can take two elements in the
-- terminal. One element encodes the character and the other elements have
-- PartOf partiality element in them, indicating they are part of that
-- other element.
-- PartOf coordinates are relative. This allows the elements to be moved
-- around without coordinates becoming invalid.
data Partiality = Independent | PartOf Int Int

data Elem = Elem { string :: String,
                   attrs :: Attributes,
                   partOf :: Partiality }

data Color = Black | Red | Green | Blue | Magenta | Yellow | Cyan | White |
             Default
             deriving(Eq, Show)

-- these map integers to corresponding colors that are used in ANSI escape
-- sequences to yield that color. Different values for foregrounds and
-- backgrounds.
fgToColor :: Int -> Maybe Color
fgToColor 30 = Just Black
fgToColor 31 = Just Red
fgToColor 32 = Just Green
fgToColor 33 = Just Yellow
fgToColor 34 = Just Blue
fgToColor 35 = Just Magenta
fgToColor 36 = Just Cyan
fgToColor 37 = Just White
fgToColor 38 = Just Default
fgToColor 39 = Just Default
fgToColor _ = Nothing

bgToColor :: Int -> Maybe Color
bgToColor 40 = Just Black
bgToColor 41 = Just Red
bgToColor 42 = Just Green
bgToColor 43 = Just Yellow
bgToColor 44 = Just Blue
bgToColor 45 = Just Magenta
bgToColor 46 = Just Cyan
bgToColor 47 = Just White
bgToColor 48 = Just Default
bgToColor 49 = Just Default
bgToColor _ = Nothing

defaultAttrs :: Attributes
defaultAttrs = Attributes Default Default False False

defaultElem :: Elem
defaultElem = Elem " " defaultAttrs Independent

emptyTerminal :: Int -> Int -> Terminal
emptyTerminal width height = Terminal (array ((1,1), (width, height))
                                      [((x,y), defaultElem) |
                                           x <- [1..width],
                                           y <- [1..height]])
                                      1 1
                                      defaultAttrs
                                      baseConsumer

handleChar :: Terminal -> Char -> Terminal
handleChar t@(Terminal { consumer = c }) = c t

width :: Terminal -> Int
width Terminal { elements = arr } = fst $ snd $ bounds arr

height :: Terminal -> Int
height Terminal { elements = arr } = snd $ snd $ bounds arr

unsafeSet :: TerminalArray -> Int -> Int -> Elem -> TerminalArray
-- Doesn't actually use unsafe operations. Also, very inefficient as it is.
-- We can't use unsafeThaw/freeze unless we are also certain the terminal
-- will not every be referenced anywhere else. Right now, we don't have
-- that guarantee.
unsafeSet arr x y elem = runST $ do marr <- (thaw arr) :: STTerminalArray s
                                    writeArray marr (x, y) elem
                                    freeze marr

baseConsumer :: Terminal -> Char -> Terminal
-- Backspace
baseConsumer t@(Terminal _ 1 1 _ _) '\x8' = t
baseConsumer t@(Terminal _ 1 y _ _) '\x8' = t { cx = width t, cy = y - 1 }
baseConsumer t@(Terminal _ x _ _ _) '\x8' = t { cx = x - 1 }
-- Carriage return
baseConsumer t '\xd' = t { cx = 1 }
-- Line feed
baseConsumer t@(Terminal _ _ y _ _) '\xa'
  | y == height t = (scrollUp t) { cy = height t }
  | otherwise     = t { cy = y + 1 }
-- Tab
baseConsumer t@(Terminal _ x _ _ _) '\x9' = t { cx = min (width t)
                                                     (x + (8 - mod x 8)) }
baseConsumer t@(Terminal elems x y _ _) '\x1b' =
  t { consumer = \t ch -> if ch == '['
                            then t { consumer = consumeCSI }
                            else baseConsumer t ch }

-- Make sure the non-printable characters from ASCII don't slip into the
-- terminal
baseConsumer t@(Terminal elems x y _ _) ch
  | ch < ' '       = t
  -- TODO: handle CJK characters and some common Unicode control code
  -- points.
  | x < width t    =  t { cx = x + 1,
                          elements = unsafeSet elems x y newElem }

  -- The lower bound check is needed to avoid infinite recursion when
  -- terminal size is ax1 or 1xb or something like that
  | x > 1 &&
    x == width t &&
    y < height t   =  t { cx = 1, cy = y + 1,
                          elements = unsafeSet elems x y newElem }

  | x > 1 &&
    y > 1 &&
    x == width t &&
    y == height t  = baseConsumer (scrollUp t) ch

  | otherwise = t { elements = unsafeSet elems x y newElem }
  where
    newElem = Elem [ch] (attributes t) Independent

type Consumer = Terminal -> Char -> Terminal

consumeCSI :: Consumer
consumeCSI t ch
  | isDigit ch    = consumeNumberSequence applyCSI t ch
  | ch == '?'     = t { consumer = consumeQuestionCSI }
  | otherwise     = t { consumer = baseConsumer }

consumeQuestionCSI :: Consumer
consumeQuestionCSI = consumeNumberSequence applyQuestionCSI

applyQuestionCSI :: [Int] -> Consumer
applyQuestionCSI _ t _ = t { consumer = baseConsumer }

consumeNumberSequence :: ([Int] -> Consumer) -> Consumer
consumeNumberSequence cont = accum []
  where
    accumAdd xs int = accum (int:xs)
    accum xs = \t ch -> if isDigit ch
                          then consumeInteger (accumAdd xs) t ch
                          else if ch == ';' then t { consumer = (accum xs) }
                                            else cont (reverse xs) t ch

consumeInteger :: (Int -> Consumer) -> Consumer
consumeInteger cont = accum 0
  where
    accum v t ch
      | isDigit ch = t { consumer = accum (v * 10 + (digitToInt ch)) }
      | otherwise  = (cont v) t ch

applyCSI :: [Int] -> Consumer
-- Attribute set
applyCSI a1 a2 a3 = (applyCSI2 a1 a2 a3) { consumer = baseConsumer }
  where
    applyCSI2 [] t 'm' = applyAttrib t 0
    applyCSI2 xs t 'm' = foldl applyAttrib t xs
    applyCSI2 []  t 'J'   = eraseBelow t
    applyCSI2 (0:_) t 'J' = eraseBelow t
    applyCSI2 (1:_) t 'J' = eraseAbove t
    applyCSI2 (2:_) t 'J' = eraseAll t
    applyCSI2 _ t 'J'     = eraseBelow t
    applyCSI2 [row,column] t 'H' = t { cx = column, cy = row }
    applyCSI2 [row] t 'H' = t { cx = 1, cy = row }
    applyCSI2 _ t 'H' = t { cx = 1, cy = 1 }
    applyCSI2 xs t ch = error $ (show xs) ++ (show ch)

eraseBelow :: Terminal -> Terminal
eraseBelow t@(Terminal { elements = elems, cx = x, cy = y }) =
  t { elements = newElems }
  where
    newElems =
      runST $ do marr <- thaw elems :: STTerminalArray s
                 mapM_ (\ex -> writeArray marr (ex, y) curElem) [x+1..w]
                 mapM_ (\ex -> mapM_ (\ey ->
                   writeArray marr (ex, ey) curElem) [y+1..h]) [1..w]
                 freeze marr
    w = width t
    h = height t
    curElem = currentElem t

eraseAbove :: Terminal -> Terminal
eraseAbove t@(Terminal { elements = elems, cx = x, cy = y }) =
  t { elements = newElems }
  where
    newElems =
      runST $ do marr <- thaw elems :: STTerminalArray s
                 mapM_ (\ex -> writeArray marr (ex, y) curElem) [x,x-1..1]
                 mapM_ (\ex -> mapM_ (\ey ->
                   writeArray marr (ex, ey) curElem) [y-1,y-2..1]) [1..w]
                 freeze marr
    w = width t
    h = height t
    curElem = currentElem t

eraseAll :: Terminal -> Terminal
eraseAll = eraseAbove . eraseBelow

applyAttrib :: Terminal -> Int -> Terminal
applyAttrib t@(Terminal { attributes = attrs }) n
  | fgToColor n /= Nothing =
      t { attributes = attrs { foreground = fromJust $ fgToColor n } }
  | bgToColor n /= Nothing =
      t { attributes = attrs { foreground = fromJust $ bgToColor n } }
  | otherwise = t

currentElem :: Terminal -> Elem
currentElem t = Elem " " (attributes t) Independent

scrollUp :: Terminal -> Terminal
scrollUp t@(Terminal elems x y attrs _)
  | height t == 1    = clearLine t y
  | otherwise        =
       t { cy = max 1 (y - 1), elements = moveElemsUp elems }
       where
         w = width t
         h = height t
         moveElemsUp elems =
           runST $ do marr <- thaw elems :: STTerminalArray s
                      (mapM_
                        (\y -> mapM_ (\x -> readArray marr (x, y) >>=
                                            writeArray marr (x, y-1))
                                     [1..w])
                          [h, h-1..2])

                      (mapM_
                        (\x -> writeArray marr (x, h) (currentElem t))
                        [1..w])

                      freeze marr

clearLine :: Terminal -> Int -> Terminal
clearLine t y = t { elements = clearElemLine elems y }
                where
                  elems = elements t
                  w = width t

                  clearElemLine elems line =
                    runST $ do marr <- thaw elems :: STTerminalArray s
                               (mapM_ (\x -> writeArray marr (x, y)
                                               (currentElem t)) [1..w])
                               freeze marr

-- For debugging
printOut :: Terminal -> IO ()
printOut t@(Terminal { elements = elems }) =
    mapM_ (\y -> (mapM_ (\x -> putStr (string (elems ! (x, y)))) [1..w]) >>
                 putChar '\n') [1..h]
    where
      w = width t
      h = height t


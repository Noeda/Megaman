module Terminal(Terminal(elements), emptyTerminal, isSomewhereOnScreen,
                isSomewhereOnScreenPos, strAt, width, height,
                Attributes(foreground, background, bold, inverse),
                cursorIsInside, captureString, captureInteger,
                Elem(string, attrs),
                Color(..),
                handleChar, printOut) where

import Data.Char
import Data.Array
import Control.Monad
import Control.Monad.ST
import Data.Array.ST
import Data.Array.MArray
import Data.Maybe
import Data.List(isPrefixOf)

import Safe

import Text.Regex.TDFA.String as R
import Text.Regex.Base.RegexLike as RL

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
  | ch == '?'     = t { consumer = consumeQuestionCSI }
  | otherwise     = consumeNumberSequence applyCSI t ch

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
    applyCSI2 (1:_) t 'K' = eraseLeft t
    applyCSI2 (2:_) t 'K' = eraseLeftAndRight t
    applyCSI2 _ t 'K'     = eraseRight t
    applyCSI2 [x] t 'C'   = t { cx = min w (cx1 + x) }
    applyCSI2 _ t 'C'     = t { cx = min w (cx1 + 1) }
    applyCSI2 [x] t 'D'   = t { cx = max 1 (cx1 - x) }
    applyCSI2 _ t 'D'     = t { cx = max 1 (cx1 - 1) }
    applyCSI2 [x] t 'A'   = t { cy = max 1 (cy1 - x) }
    applyCSI2 _ t 'A'     = t { cy = max 1 (cy1 - 1) }
    applyCSI2 [x] t 'B'   = t { cy = min h (cy1 + x) }
    applyCSI2 _ t 'B'     = t { cy = min h (cy1 + 1) }
    applyCSI2 xs t ch = error $ (show xs) ++ (show ch)

    cx1 = cx a2
    cy1 = cy a2
    w = width a2
    h = height a2

eraseFromList :: Terminal -> [(Int, Int)] -> Terminal
eraseFromList t indices =
  t { elements = newElems }
  where
    newElems =
      runST $ do marr <- thaw elems :: STTerminalArray s
                 mapM_ (\(x, y) -> writeArray marr (x, y) curElem) indices
                 freeze marr
    elems = elements t
    w = width t
    h = height t
    curElem = currentElem t

eraseLeft :: Terminal -> Terminal
eraseLeft t = eraseFromList t [(x, cy1) | x <- [1..cx1]]
  where
    cy1 = cy t
    cx1 = cx t

eraseRight :: Terminal -> Terminal
eraseRight t = eraseFromList t [(x, cy1) | x <- [cx1..w]]
  where
    cy1 = cy t
    cx1 = cx t
    w = width t

eraseLeftAndRight :: Terminal -> Terminal
eraseLeftAndRight = eraseLeft . eraseRight

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

yLineToStr :: Int -> Terminal -> String
yLineToStr row t =
  foldr (++) [] (map string [elems ! (x, row) | x <- [1..w]])
  where
    w = width t
    elems = elements t

isSomewhereOnScreenPos :: String -> Terminal -> [(Int, Int)]
isSomewhereOnScreenPos str t =
  -- FIXME: inefficient. Can be sped up for common cases.
  foldl (\result x -> foldl (\result y -> matchesStringAt result x y) result
                            [1..h])
        [] [1..w]
  where
    w = width t
    h = height t
    elems = elements t
    matchesStringAt result x y =
      let pstr = foldr (++) [] (map string [elems ! (x1, y) | x1 <- [x..w]])
       in if isPrefixOf str pstr
             then (x,y):result
             else result

isSomewhereOnScreen :: String -> Terminal -> Bool
isSomewhereOnScreen str t =
  case isSomewhereOnScreenPos str t of
    []      -> False
    _       -> True

linesOf :: (Int, Int) -> (Int, Int) -> Terminal -> [String]
linesOf (left, top) (right, bottom) t =
  [(foldr (++) [] (map string [elems ! (x, row) | x <- [left..right]]))
   | row <- [top..bottom]]
  where
    elems = elements t

-- just like Prelude.any but returns the item as Maybe a
anyValue :: (a -> Maybe b) -> [a] -> Maybe b
anyValue _ [] = Nothing
anyValue predicate (i:rest) = case predicate i of
                                Nothing -> anyValue predicate rest
                                x       -> x

captureString :: String -> (Int, Int) -> (Int, Int) -> Terminal -> Maybe String
captureString str topleft rightbottom t =
  case R.compile RL.defaultCompOpt RL.defaultExecOpt str of
    (Left msg) -> error $ "Regex compilation error: " ++ msg -- I want to know
                                                             -- if our regexes
                                                             -- are wrong
    (Right regex) ->
      anyValue (\line ->
                 case R.execute regex line of
                      Left msg -> error $ "Regex execution error: " ++ msg
                      Right Nothing -> Nothing
                      Right (Just matches) ->
                                       if len < 1
                                         then errorNoSubmatch
                                         else Just $
                                              take (snd (matches ! 1))
                                                (drop
                                                 (fst (matches ! 1))
                                                 str)
                                        where
                                          len = snd (bounds matches))
               $ linesOf topleft rightbottom t
  where
    errorNoSubmatch = error $
      "Expected regex to return at least one submatch. " ++
      "(offending regex: " ++ str ++ ")"

captureInteger :: String -> (Int, Int) -> (Int, Int) -> Terminal -> Maybe Int
captureInteger str b1 b2 t =
  case captureString str b1 b2 t of
    Nothing  -> Nothing
    Just str -> readMay str :: Maybe Int

cursorIsInside :: (Int, Int) -> (Int, Int) -> Terminal -> Bool
cursorIsInside (left, top) (right, bottom) t =
  left <= x && top <= y && right >= x && bottom >= y
  where
    x = cx t
    y = cy t

-- For debugging
printOut :: Terminal -> IO ()
printOut t@(Terminal { elements = elems }) = do
    mapM_ (\x -> putChar '-') [1..(w+2)]
    putChar '\n'
    mapM_ (\y -> putChar '|' >>
                 (mapM_ (\x -> putStr (string (elems ! (x, y)))) [1..w]) >>
                 putChar '|' >>
                 putChar '\n') [1..h]
    mapM_ (\x -> putChar '-') [1..(w+2)]
    putChar '\n'
    where
      w = width t
      h = height t

strAt :: (Int, Int) -> Terminal -> String
strAt (x, y) (Terminal { elements = elems }) = string $ elems ! (x,y)


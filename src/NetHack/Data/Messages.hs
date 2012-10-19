module NetHack.Data.Messages(stripMessages, trim) where

import qualified Terminal.Data as T
import qualified Terminal.Terminal as T

-- Okay, I hope I got NetHack more behaviour correct. Things seem like
-- this:
--
-- Most of the time, all the messages are on top. If NetHack can show all
-- the messages at once, then there may not be '--More--' on the screen.
--
-- If the messages don't fit, it shows the first messages and then shows
-- '--More--'.
--
-- Sometimes there can be two (in pathological cases even more?) lines for
-- messages. In that case, '--More--' is always shown.
--
-- There are always two spaces between messages.
--
-- I didn't sourcedive for the behaviour; I just checked in wizard mode how
-- it seems to work.
stripMessages :: T.Terminal -> [String]
stripMessages t =
  -- If there's a '--More--', great! We know everything before that is a
  -- message.
  case T.isSomewhereOnScreenPos "--More--" t of
    []         -> stripMessagesWithoutMore (1, 1) (w, 1) w t
    ((x, y):_) -> stripMessagesWithoutMore (1, 1) (w, y) x t
  where
    w = T.width t

stripMessagesWithoutMore :: (Int, Int) -> (Int, Int) -> Int ->
                            T.Terminal -> [String]
stripMessagesWithoutMore (left, top) (right, bottom) lastX t =
  let (strs, str, lastch) = (foldl (\(accum, str, lastch) ch ->
                   if isWhitespace lastch && isWhitespace ch
                     then if not $ isWhitespace str
                            then (reverse str:accum, [], "\x01")
                            else (accum, [], "\x01")
                     else (accum, ch++str, ch))
                 ([], [], "\x01") $ map (: []) $
                 collapseNewLines $ concat
                  ([T.strAt (x1, y1) t | y1 <- [top..bottom-1],
                                         x1 <- [left..right]] ++
                   [T.strAt (x1, bottom) t | x1 <- [left..(lastX-1)]]))
      strs2 = reverse $ if not $ isWhitespace str then reverse str:strs
                                                  else strs
   in map trim strs2

-- Turns any whitespace sequence in a string that has carriage returns or
-- newlines into one space character.
collapseNewLines :: String -> String
collapseNewLines [] = []
collapseNewLines str = acc [] str
  where
   acc accum []         = reverse accum
   acc accum s@(' ':_)  = reverse accum ++ collapseWS s [] False
   acc accum s@('\n':_) = reverse accum ++ collapseWS s [] False
   acc accum s@('\r':_) = reverse accum ++ collapseWS s [] False
   acc accum (x:xs)     = acc (x:accum) xs

   collapseWS [] acc _        = reverse acc
   collapseWS (' ':xs) acc x  = collapseWS xs (' ':acc) x
   collapseWS ('\n':xs) acc _ = collapseWS xs [] True
   collapseWS ('\r':xs) acc _ = collapseWS xs [] True
   collapseWS xs acc False    = reverse acc ++ collapseNewLines xs
   collapseWS xs acc True     = ' ':collapseNewLines xs

trim :: String -> String
trim = reverse . dropWhile isWhitespaceCh .
       reverse . dropWhile isWhitespaceCh

isWhitespaceCh :: Char -> Bool
isWhitespaceCh ' '  = True
isWhitespaceCh '\n' = True
isWhitespaceCh '\r' = True
isWhitespaceCh _    = False

isWhitespace :: String -> Bool
isWhitespace [] = False
isWhitespace (x:xs)
  | xs == []         = isWhitespaceCh x
  | otherwise        = isWhitespaceCh x && isWhitespace xs




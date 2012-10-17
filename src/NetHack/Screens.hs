module NetHack.Screens where

import Control.Monad
import NetHack.Monad.NHAction
import NetHack.LogicPlumbing
import System.IO.Unsafe
import qualified Terminal as T

ifIn :: NHAction Bool -> NHAction a -> NHAction (Either a ())
ifIn testaction dothis = do
  result <- testaction
  case result of
    True  -> dothis >>= (return . Left)
    False -> return $ Right ()

ifNotIn :: NHAction Bool -> NHAction a -> NHAction (Either a ())
ifNotIn testaction dothis = do
  result <- testaction
  case (not result) of
    True  -> dothis >>= (return . Left)
    False -> return $ Right ()

isSomewhereOnScreen :: String -> NHAction Bool
isSomewhereOnScreen str = do
  terminal <- getTerminal
  return $ T.isSomewhereOnScreen str terminal

shallIPick       = isSomewhereOnScreen "Shall I pick a character's"
pickARole        = isSomewhereOnScreen "Pick a role for your character"
pickTheRace      = isSomewhereOnScreen "Pick the race of your"
pickTheGender    = isSomewhereOnScreen "Pick the gender of your"
pickTheAlignment = isSomewhereOnScreen "Pick the alignment of your"
itIsWrittenInTheBook = isSomewhereOnScreen "It is written in the Book of"

morePrompt = isSomewhereOnScreen "--More--"
restoringSave = isMessageOnScreen "Restoring save file..."

gameScreen = do t <- getTerminal
                let b1 = T.cursorIsInside (1, 2) (80, 22) t
                b2 <- morePrompt
                return $ b1 && (not b2)

isMessageOnScreen :: String -> NHAction Bool
isMessageOnScreen str = do ns <- get; return (str `elem` messages ns)

welcomeBackToNetHack = liftM2 (||)
                         (isSomewhereOnScreen ", welcome to NetHack!")
                         (isSomewhereOnScreen ", welcome back to NetHack!")



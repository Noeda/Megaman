module NetHack.Control.Screen
  (cursorIsAt, ifIn, ifNotIn,
   isSomewhereOnScreen,
   shallIPick,
   pickARole,
   pickTheRace,
   pickTheGender,
   pickTheAlignment,
   itIsWrittenInTheBook,
   welcomeBackToNetHack,
   gameScreen,
   restoringSave,
   morePrompt,
   doYouWantToKeepSaveFile,
   doYouWantYourPossessionsIdentified,
   optionalDie)
  where

import NetHack.Monad.NHAction
import NetHack.Data.NetHackState(messages)
import qualified Terminal.Data as T
import qualified Terminal.Terminal as T
import Control.Monad

cursorIsAt :: Int -> Int -> NHAction Bool
cursorIsAt x y = do
  t <- getTerminalM
  return $ x == T.cursorX t &&
           y == T.cursorY t

ifIn :: NHAction Bool -> NHAction a -> NHAction (Either a ())
ifIn testaction dothis = do
  result <- testaction
  if result then liftM Left dothis
            else return $ Right ()

ifNotIn :: NHAction Bool -> NHAction a -> NHAction (Either a ())
ifNotIn testaction dothis = do
  result <- testaction
  if not result then liftM Left dothis
                else return $ Right ()

isSomewhereOnScreen :: String -> NHAction Bool
isSomewhereOnScreen str = do
  terminal <- getTerminalM
  return $ T.isSomewhereOnScreen str terminal

shallIPick       = isSomewhereOnScreen "Shall I pick a character's"
pickARole        = isSomewhereOnScreen "Pick a role for your character"
pickTheRace      = isSomewhereOnScreen "Pick the race of your"
pickTheGender    = isSomewhereOnScreen "Pick the gender of your"
pickTheAlignment = isSomewhereOnScreen "Pick the alignment of your"
itIsWrittenInTheBook = isSomewhereOnScreen "It is written in the Book of"

doYouWantToKeepSaveFile =
  isSomewhereOnScreen "Do you want to keep the save file?"

doYouWantYourPossessionsIdentified =
  isSomewhereOnScreen "Do you want your possessions identified?"

optionalDie = isSomewhereOnScreen "Die? [yn]"

morePrompt = isSomewhereOnScreen "--More--"
restoringSave = isMessageOnScreen "Restoring save file..."

gameScreen = do t <- getTerminalM
                let b1 = T.cursorIsInside (1, 2) (80, 22) t
                b2 <- morePrompt
                return $ b1 && not b2

isMessageOnScreen :: String -> NHAction Bool
isMessageOnScreen str = do ns <- get; return (str `elem` messages ns)

welcomeBackToNetHack = liftM2 (||)
                         (isSomewhereOnScreen ", welcome to NetHack!")
                         (isSomewhereOnScreen ", welcome back to NetHack!")


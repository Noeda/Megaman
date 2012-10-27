module NetHack.Monad.NHAction
  (runNHAction, answer, NHAction(), update, getTerminalM, get,
   putInventoryM, putInventoryNeedsUpdateM, getElementM, putElementM,
   getMessagesM, control, nextRunningIDM,
   getLevelM, bailout, getCoordsM, forbidMovementFromM,
   putLevelM, putCurrentLevelM,
   maybeMarkAsOpenDoorM,
   goingDownstairs, getLevelTransitionM, resetLevelTransitionM,
   Answerable())
  where

import NetHack.Data.LevelTransition
import NetHack.Data.Level
import qualified NetHack.Data.NetHackState as NS
import NetHack.Data.Messages
import NetHack.Data.Item(Item)

import qualified Terminal.Data as T
import qualified Terminal.Terminal as T
import Communication.RWChan

import qualified Data.Map as M

import Control.Concurrent.STM
import Control.Monad.State
import qualified Data.ByteString.Char8 as B

newtype NHAction a = NHAction (StateT NS.NetHackState IO a)
                     deriving (MonadState NS.NetHackState, MonadIO, Functor)

instance Monad NHAction where
  return x = NHAction $ return x
  (NHAction m) >>= b  = NHAction $ m >>= (\value -> let x = b value
                                                     in unwrap x)
                        where
                          unwrap (NHAction x) = x

runNHAction :: NS.NetHackState -> NHAction a -> IO a
runNHAction ns (NHAction st) = do
  (result, _) <- runStateT st ns
  return result

getTerminalM :: NHAction T.Terminal
getTerminalM = do ns <- get; return $ NS.terminal ns

getLevelM :: NHAction Level
getLevelM = do ns <- get; return $ NS.currentLevel ns

putLevelM :: Level -> NHAction ()
putLevelM l = do ns <- get; put $ NS.setLevel ns l

putCurrentLevelM :: Level -> NHAction ()
putCurrentLevelM l = do ns <- get; put $ NS.setCurrentLevel ns l

getMessagesM :: NHAction [String]
getMessagesM = do ns <- get; return $ NS.messages ns

getCoordsM :: NHAction (Int, Int)
getCoordsM = do t <- getTerminalM; return $ T.coords t

getElementM :: (Int, Int) -> NHAction Element
getElementM coords = do level <- getLevelM
                        return $ elemAtDefault level coords

putElementM :: Element -> (Int, Int) -> NHAction ()
putElementM elem coords = do level <- getLevelM
                             let elems = elements level
                             putLevelM $ setElements level $
                               M.insert coords elem elems

putInventoryM :: M.Map Char [Item] -> NHAction ()
putInventoryM i = do ns <- get; put $ NS.setInventory ns i

putInventoryNeedsUpdateM :: Bool -> NHAction ()
putInventoryNeedsUpdateM b =
  do ns <- get; put $ NS.setInventoryNeedsUpdate ns b

nextRunningIDM :: NHAction Int
nextRunningIDM = do
  ns <- get
  let (id, newNs) = NS.nextRunningID ns
  put newNs
  return id

getLevelTransitionM :: NHAction (Maybe LevelTransition)
getLevelTransitionM = do ns <- get; return $ NS.levelTransition ns

resetLevelTransitionM :: NHAction ()
resetLevelTransitionM = do ns <- get; put $ NS.resetLevelTransition ns

goingDownstairs :: NHAction ()
goingDownstairs = do
  l <- getLevelM
  coords <- getCoordsM
  ns <- get
  let dLevel = NS.dungeonLevel ns
  put $ NS.setLevelTransition ns $ goingDownstairsTransition dLevel coords

maybeMarkAsOpenDoorM :: Coords -> NHAction ()
maybeMarkAsOpenDoorM coords =
  do t <- getTerminalM
     level <- getLevelM
     putLevelM $ maybeMarkAsOpenDoor level t coords

forbidMovementFromM :: (Int, Int) -> (Int, Int) -> NHAction ()
forbidMovementFromM from to =
  do level <- getLevelM
     putLevelM $ forbidMovementFrom level from to

bailout = error

update :: NHAction ()
update = do
  oldState <- get
  newState <- liftIO $ atomically $ NS.update oldState
  liftIO $ T.printOut $ NS.terminal newState
  put newState

class Answerable a where
  answer :: a -> NHAction ()

instance Answerable Char where
  answer ch = do
    oldState <- get
    liftIO $ atomically $ NS.write oldState $ B.pack [ch]
    update

instance Answerable [Char] where
  answer str = do
    oldState <- get
    liftIO $ atomically $ NS.write oldState $ B.pack str
    update

instance Answerable B.ByteString where
  answer str = do
    oldState <- get
    liftIO $ atomically $ NS.write oldState $ str
    update

control :: Char -> String
control 'A' = "\x01"
control 'B' = "\x02"
control 'C' = "\x03"
control 'D' = "\x04"
control 'E' = "\x05"
control 'F' = "\x06"
control 'G' = "\x07"
control 'H' = "\x08"
control 'I' = "\x09"
control 'J' = "\x0a"
control 'K' = "\x0b"
control 'L' = "\x0c"
control 'M' = "\x0d"
control 'N' = "\x0e"
control 'O' = "\x0f"
control 'P' = "\x10"
control 'Q' = "\x11"
control 'R' = "\x12"
control 'S' = "\x13"
control 'T' = "\x14"
control 'U' = "\x15"
control 'V' = "\x16"
control 'W' = "\x17"
control 'X' = "\x18"
control 'Y' = "\x19"
control 'Z' = "\x1a"
control '[' = "\x1b"
control '\\' = "\x1c"
control '^' = "\x1e"
control '_' = "\x1f"
control _ =
  error $ "Invalid control character requested (" ++ show control ++ ")"


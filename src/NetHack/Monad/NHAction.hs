module NetHack.Monad.NHAction
  (runNHAction, answer, NHAction(), update, getTerminalM, get, putLevelM,
   putInventoryM, putInventoryNeedsUpdateM, getElementM, putElementM,
   getMessagesM,
   getLevelM, bailout, getCoordsM, forbidMovementFromM,
   Answerable())
  where

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


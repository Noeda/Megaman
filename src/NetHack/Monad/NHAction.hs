module NetHack.Monad.NHAction
  (runNHAction, answer, NHAction(), update, getTerminalM, get, putLevelM,
   getLevelM, bailout)
  where

import NetHack.Data.Level(Level)
import qualified NetHack.Data.NetHackState as NS
import NetHack.Data.Messages
import Terminal.Data(Terminal)
import Terminal.Terminal
import Communication.RWChan

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

getTerminalM :: NHAction Terminal
getTerminalM = do ns <- get; return $ NS.terminal ns

getLevelM :: NHAction Level
getLevelM = do ns <- get; return $ NS.currentLevel ns

putLevelM :: Level -> NHAction ()
putLevelM l = do ns <- get; put $ NS.setLevel ns l

bailout = error

update :: NHAction ()
update = do
  oldState <- get
  newState <- liftIO $ atomically $ NS.update oldState
  liftIO $ printOut $ NS.terminal newState
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


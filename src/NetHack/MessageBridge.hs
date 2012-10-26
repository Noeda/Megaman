module NetHack.MessageBridge(runNetHackBot) where

import Communication.RWChan
import NetHack.Monad.NHAction

import Prelude hiding (foldl, foldl1, mapM_)
import Data.Foldable(foldl, foldl1, mapM_)

import Control.Exception

import System.Clock(Clock(Monotonic), TimeSpec(..), getTime)

import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

import qualified Data.Sequence as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import NetHack.Data.NetHackState(newGame)
import NetHack.AI(root)

data NetHackMsg = Closed |
                  Chunk B.ByteString
type NetHackChan = TChan NetHackMsg
type NetHackWriteChan = TChan B.ByteString

data Dispatcher = Dispatcher { ptyChannels :: (NetHackChan,
                                               NetHackWriteChan),
                               nethackChannels :: RWChan B.ByteString,
                               msgQueue :: S.Seq B.ByteString,
                               lastTimeReceived :: Integer }

-- Time to wait after the last received data before running bot AI
-- (nanoseconds)
graceTime = 100000000

netHackIteratee :: MonadIO m => NetHackChan ->
                                E.Iteratee B.ByteString m ()
netHackIteratee chan = E.continue step where
  step E.EOF = do liftIO $ atomically $ writeTChan chan Closed
                  E.yield () E.EOF
  step (E.Chunks []) = E.continue step
  step (E.Chunks chunks) = do
    liftIO $ atomically $ mapM_ (writeTChan chan . Chunk) chunks
    E.continue step

runNetHackBot :: E.Enumerator B.ByteString IO () ->
                 E.Iteratee B.ByteString IO () ->
                 IO ()
runNetHackBot enumerator iteratee = do
  chan <- newTChanIO
  writerChan <- newTChanIO

  let ptyChannels = (chan, writerChan)

  nethackChannels <- newRWChanIO

  -- Make sure if brains dies, we die too.
  -- TODO: fork the enumerator/iteratee too and wait here until either of
  -- them dies.
  tid <- myThreadId

  writertid <- forkIO $ finally (runWriter iteratee writerChan)
                                (killThread tid)
  dispatchertid <- forkIO $ finally (runDispatcher ptyChannels nethackChannels)
                                    (killThread tid >> killThread writertid)
  forkIO $ finally ((runAI . flipRWChan) nethackChannels)
                   (mapM_ killThread [writertid, dispatchertid, tid])
  E.run_ $ enumerator E.$$ netHackIteratee chan

runWriter :: E.Iteratee B.ByteString IO () ->
             NetHackWriteChan ->
             IO ()
runWriter iteratee chan =
  E.run_ $ EL.repeatM (atomically $ readTChan chan) E.$$ iteratee

now :: IO Integer
now = do (TimeSpec sec nsec) <- getTime Monotonic
         let (isec, insec) = (fromIntegral sec,
                              fromIntegral nsec) :: (Integer, Integer)
         return $ isec * 1000000000 + insec

runDispatcher :: (NetHackChan, NetHackWriteChan) -> RWChan B.ByteString ->
                 IO ()
runDispatcher channels nethackChannels = do
  n <- now
  loopDispatcher (Dispatcher channels nethackChannels S.empty n)

-- What is happening here is that we call runLogic whenever we haven't
-- received data from the channel after 'graceTime' nanoseconds. That's
-- when we assume NetHack has sent as much data as it is going to send for
-- now.
loopDispatcher :: Dispatcher -> IO ()
loopDispatcher d@(Dispatcher { ptyChannels = (chan, _),
                               msgQueue = msgqueue,
                               lastTimeReceived = lastreceived }) = do
  n <- now
  chunk <- atomically $ tryReadTChan chan
  case chunk of
    Nothing -> if n - lastreceived < graceTime
                   then threadDelay
                           (fromIntegral ((n - lastreceived) `div` 1000)) >>
                        loopDispatcher d
                   else do d2 <- runLogic d
                           n <- now
                           loopDispatcher d2 { lastTimeReceived = n }
    Just (Chunk msg) -> loopDispatcher (d { lastTimeReceived = n,
                                           msgQueue = msgqueue S.|> msg })

runLogic :: Dispatcher -> IO Dispatcher
runLogic = atomically . flushMsgQueueToTerminal

flushMsgQueueToTerminal :: Dispatcher -> STM Dispatcher
flushMsgQueueToTerminal d@(Dispatcher { msgQueue = queue,
                                        nethackChannels = nhchan }) = do
  flushReadQueue d
  if queue == S.empty
    then return d
    else do let completestr = foldl1 B.append queue
            writeRWChan nhchan completestr
            return d { msgQueue = S.empty }

flushReadQueue :: Dispatcher -> STM ()
flushReadQueue d@(Dispatcher { ptyChannels = (_, chan),
                               nethackChannels = nhchan }) = do
  msg <- tryReadRWChan nhchan
  case msg of
    Nothing  -> return ()
    Just str -> writeTChan chan str >> flushReadQueue d

runAI :: RWChan B.ByteString -> IO ()
runAI channels = runNHAction (newGame channels) root


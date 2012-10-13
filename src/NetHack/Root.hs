module NetHack.Root(runNetHackBot) where

import Prelude hiding (foldl)
import Data.Foldable(foldl)

import qualified Terminal as T

import qualified Data.ByteString.Char8 as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.Clock
import qualified Data.Sequence as S

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

-- To be written
data NetHackState = NetHackState { terminal :: T.Terminal }

data NetHackMsg = Closed |
                  Chunk B.ByteString
type NetHackChan = TChan NetHackMsg
type NetHackWriteChan = TChan B.ByteString

data Dispatcher = Dispatcher { state :: NetHackState,
                               readChan :: NetHackChan,
                               writeChan :: NetHackWriteChan,
                               msgQueue :: S.Seq B.ByteString,
                               lastTimeReceived :: Integer }

-- Time to wait after the last received data before running bot AI
-- (nanoseconds)
graceTime = 500000000

newGame :: NetHackState
newGame = NetHackState { terminal = T.emptyTerminal 80 24 }

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

  -- Make sure if brains dies, we die too.
  -- TODO: fork the enumerator/iteratee too and wait here until either of
  -- them dies.
  tid <- myThreadId

  writertid <- forkIO $ runWriter iteratee writerChan
  forkIO $ finally (runDispatcher chan writerChan)
                   (killThread tid >> killThread writertid)
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

runDispatcher :: NetHackChan -> NetHackWriteChan -> IO ()
runDispatcher chan writeChan = do
  n <- now
  loopDispatcher (Dispatcher newGame chan writeChan S.empty n)

-- What is happening here is that we call runLogic whenever we haven't
-- received data from the channel after 'graceTime' nanoseconds. That's
-- when we assume NetHack has sent as much data as it is going to send for
-- now.
loopDispatcher :: Dispatcher -> IO ()
loopDispatcher d@(Dispatcher game chan _ msgqueue lastreceived) = do
  n <- now
  chunk <- atomically $ tryReadTChan chan
  case chunk of
    Nothing -> if n - lastreceived < graceTime
                   then (threadDelay
                           (fromIntegral ((n - lastreceived) `div` 1000))) >>
                        loopDispatcher d
                   else do d2 <- runLogic d
                           n <- now
                           loopDispatcher d2 { lastTimeReceived = n }
    Just (Chunk msg) -> loopDispatcher (d { lastTimeReceived = n,
                                           msgQueue = msgqueue S.|> msg })

runLogic :: Dispatcher -> IO Dispatcher
-- Update the terminal and then run the game logic
runLogic = runGameLogic . flushMsgQueueToTerminal

flushMsgQueueToTerminal :: Dispatcher -> Dispatcher
flushMsgQueueToTerminal d@(Dispatcher state _ _ queue _) =
  d { state = flush state queue, msgQueue = S.empty } where

    flush state queue
      | queue == S.empty = state
      | otherwise        = state { terminal =
                                     foldl (\t bs ->
                                       foldl T.handleChar t (B.unpack bs))
                                         t queue }
                           where t = terminal state

runGameLogic :: Dispatcher -> IO Dispatcher
runGameLogic d@(Dispatcher (NetHackState t) _ _ _ _) = T.printOut t >> return d


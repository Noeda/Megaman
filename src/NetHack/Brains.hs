module NetHack.Brains(runNetHackBot) where

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.Clock
import qualified Data.Sequence as S

import Control.Exception
import Control.Monad.IO.Class
import qualified Data.Enumerator as E

-- To be written
data NetHackState = NetHackState

data NetHackMsg = Closed |
                  Chunk B.ByteString
type NetHackChan = TChan NetHackMsg

data Dispatcher = Dispatcher { state :: NetHackState,
                               readChan :: NetHackChan,
                               msgQueue :: S.Seq B.ByteString,
                               lastTimeReceived :: Integer }

-- Time to wait after the last received data before running bot AI
-- (nanoseconds)
graceTime = 500000000

newGame :: NetHackState
newGame = NetHackState

netHackIteratee :: MonadIO m => NetHackChan ->
                                E.Iteratee B.ByteString m ()
netHackIteratee chan = E.continue step where
  step E.EOF = do liftIO $ atomically $ writeTChan chan Closed
                  E.yield () E.EOF
  step (E.Chunks []) = E.continue step
  step (E.Chunks chunks) = do
    liftIO $ atomically $ mapM_ (writeTChan chan . Chunk) chunks
    E.continue step

runNetHackBot :: MonadIO m => E.Enumerator B.ByteString m () ->
                              E.Iteratee B.ByteString m () ->
                              m ()
runNetHackBot enumerator iteratee = do
  chan <- liftIO (do
      chan <- newTChanIO

      -- Make sure if brains dies, we die too.
      -- TODO: fork the enumerator/iteratee too and wait here until either of
      -- them dies.
      tid <- myThreadId

      forkIO $ finally (brains chan) (killThread tid)
      return chan)
  E.run_ $ enumerator E.$$ netHackIteratee chan


now :: IO Integer
now = do (TimeSpec sec nsec) <- getTime Monotonic
         let (isec, insec) = (fromIntegral sec,
                              fromIntegral nsec) :: (Integer, Integer)
         return $ isec * 1000000000 + insec

-- The BRAINS
brains :: NetHackChan -> IO ()
brains chan = do n <- now
                 runDispatcher (Dispatcher newGame chan S.empty n)

runDispatcher :: Dispatcher -> IO ()
runDispatcher d@(Dispatcher game chan msgqueue lastreceived) = do
  n <- now
  chunk <- atomically $ tryReadTChan chan
  case chunk of
    Nothing -> if n - lastreceived < graceTime
                   then (threadDelay
                           (fromIntegral ((n - lastreceived) `div` 1000))) >>
                        runDispatcher d
                   else runLogic d { lastTimeReceived = n } >>= runDispatcher 
    Just (Chunk msg) -> runDispatcher (d { lastTimeReceived = n,
                                           msgQueue = msgqueue S.|> msg })

-- Logic here
runLogic :: Dispatcher -> IO Dispatcher
runLogic d = return d


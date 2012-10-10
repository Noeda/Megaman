module NetHack.Brains(brains,NetHackMsg(Closed,Chunk),NetHackChan) where

import qualified Data.ByteString as B
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import System.Clock
import qualified Data.Sequence as S

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


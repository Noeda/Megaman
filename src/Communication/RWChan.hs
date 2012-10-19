module Communication.RWChan(RWChan(),
                            newRWChanIO,
                            flipRWChan,
                            writeRWChan,
                            readRWChan,
                            tryReadRWChan)
  where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TChan

type RWChan a = (TChan a, TChan a)

newRWChanIO :: IO (RWChan a)
newRWChanIO = liftM2 (,) newTChanIO newTChanIO

flipRWChan :: RWChan a -> RWChan a
flipRWChan (c1, c2) = (c2, c1)

writeRWChan :: RWChan a -> a -> STM ()
writeRWChan (_, c) = writeTChan c

readRWChan :: RWChan a -> STM a
readRWChan (c, _) = readTChan c

tryReadRWChan :: RWChan a -> STM (Maybe a)
tryReadRWChan (c, _) = tryReadTChan c


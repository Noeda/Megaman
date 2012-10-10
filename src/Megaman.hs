module Main where
import NetHack.Root

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import System.Posix.Process
import System.Posix.Terminal
import System.Posix.IO.ByteString
import System.Posix.Types
import System.IO
import Foreign.C.Types

import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.ByteString.Char8 as B

main = do
  (master, slave) <- openPseudoTerminal
  forkProcess $ closeFd master >> slaveProcess slave
  closeFd slave
  runNetHackInFd master

slaveProcess :: Fd -> IO ()
slaveProcess slave = do
  createSession
  let fds = map Fd [0, 1, 2]
  mapM_ closeFd fds
  mapM_ (dupTo slave) fds
  closeFd slave
  -- How to set controlling terminal? How do I get ioctl(TIOCSCTTY, 1)?
  void $ executeFile "nethack" True ["-u", "Megaman"] Nothing

runNetHackInFd :: Fd -> IO ()
runNetHackInFd master = do
  h <- fdToHandle master
  let (enumerator, iteratee) = (EB.enumHandle 4096 h,
                                EB.iterHandle h)
  runNetHackBot enumerator iteratee


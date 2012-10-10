module NetHack.Root(runNetHackBot) where

import NetHack.Brains

import Control.Monad
import Control.Monad.IO.Class

import Control.Monad.STM
import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM.TChan

import qualified Data.Enumerator as E
import qualified Data.ByteString.Char8 as B

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


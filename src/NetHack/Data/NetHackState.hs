module NetHack.Data.NetHackState
  (NetHackState(), currentLevel, terminal, messages, update, write, newGame, setLevel)
  where

import qualified Data.ByteString.Char8 as B
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Communication.RWChan(RWChan, readRWChan, writeRWChan)
import NetHack.Data.Level(Level, newLevel)
import NetHack.Data.Messages
import Terminal.Data(Terminal)
import Terminal.Terminal(emptyTerminal, handleChar)

data NetHackState = NetHackState { currentLevel :: Level,
                                   terminal :: Terminal,
                                   messages :: [String],
                                   runningId :: Int,
                                   channels :: RWChan B.ByteString }

newGame :: RWChan B.ByteString -> NetHackState
newGame = NetHackState level (emptyTerminal 80 24) [] 1
          where
            (level, _) = newLevel 0

setTerminal :: NetHackState -> Terminal -> NetHackState
setTerminal ns t = ns { terminal = t }

setLevel :: NetHackState -> Level -> NetHackState
setLevel ns t = ns { currentLevel = t }

update :: NetHackState -> STM NetHackState
update ns@(NetHackState { channels = chan,
                          terminal = oldTerminal }) = do
  newData <- readRWChan chan
  let newTerminal = B.foldl handleChar oldTerminal newData
  return $ ns { terminal = newTerminal,
                messages = stripMessages newTerminal }

write :: NetHackState -> B.ByteString -> STM ()
write ns@(NetHackState { channels = chan }) str =
  writeRWChan chan str


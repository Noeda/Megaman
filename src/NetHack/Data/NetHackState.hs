module NetHack.Data.NetHackState
  (NetHackState(), currentLevel, terminal, messages,
   update, write, newGame, setLevel, setInventory, setInventoryNeedsUpdate,
   inventory, inventoryNeedsUpdate)
  where

import qualified Data.ByteString.Char8 as B
import Control.Concurrent.STM
import Control.Monad.IO.Class
import NetHack.Data.Item
import Communication.RWChan(RWChan, readRWChan, writeRWChan)
import NetHack.Data.Level(Level, newLevel)
import NetHack.Data.Messages
import Terminal.Data(Terminal)
import Terminal.Terminal(emptyTerminal, handleChar)
import qualified Data.Map as M

data NetHackState = NetHackState { currentLevel :: Level,
                                   terminal :: Terminal,
                                   messages :: [String],
                                   inventory :: M.Map Char Item,
                                   inventoryNeedsUpdate :: Bool,
                                   runningId :: Int,
                                   channels :: RWChan B.ByteString }

newGame :: RWChan B.ByteString -> NetHackState
newGame = NetHackState level (emptyTerminal 80 24) [] M.empty True 1
          where
            (level, _) = newLevel 0

setInventory :: NetHackState -> M.Map Char Item -> NetHackState
setInventory ns map = ns { inventory = map }

setInventoryNeedsUpdate :: NetHackState -> Bool -> NetHackState
setInventoryNeedsUpdate ns up = ns { inventoryNeedsUpdate = up }

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


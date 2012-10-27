module NetHack.Data.NetHackState
  (NetHackState(), currentLevel, terminal, messages,
   update, write, newGame, setLevel, setInventory, setInventoryNeedsUpdate,
   inventory, inventoryNeedsUpdate, nextRunningID,
   dungeonLevel, setCurrentLevel,
   setLevelTransition,
   resetLevelTransition,
   updateDungeonLevel,
   levelTransition)
  where

import qualified Data.ByteString.Char8 as B
import Control.Concurrent.STM
import Control.Monad.IO.Class
import Data.Maybe(fromJust)
import NetHack.Data.Item
import NetHack.Data.LevelTransition
import Communication.RWChan(RWChan, readRWChan, writeRWChan)
import NetHack.Data.Level(Level, newLevel, levelID)
import NetHack.Data.Messages
import qualified Terminal.Data as T
import qualified Terminal.Terminal as T
import qualified Data.Map as M

data NetHackState = NetHackState { currentLevelID :: Int,
                                   levels :: M.Map Int Level,
                                   terminal :: T.Terminal,
                                   messages :: [String],
                                   inventory :: M.Map Char [Item],
                                   inventoryNeedsUpdate :: Bool,
                                   runningID :: Int,
                                   channels :: RWChan B.ByteString,
                                   dungeonLevel :: Int,
                                   levelTransition :: Maybe LevelTransition }

currentLevel :: NetHackState -> Level
currentLevel ns = fromJust $ M.lookup id (levels ns)
                  where
                    id = currentLevelID ns

nextRunningID :: NetHackState -> (Int, NetHackState)
nextRunningID ns = (id, ns { runningID = id+1 })
                   where
                     id = runningID ns + 1

captureLevelFromScreen :: T.Terminal -> Maybe Int
captureLevelFromScreen = T.captureInteger "Dlvl:([0-9]+)" (1, 23) (80, 24)

updateDungeonLevel :: NetHackState -> T.Terminal -> NetHackState
updateDungeonLevel ns t =
  case captureLevelFromScreen t of
    Nothing -> ns
    Just lv -> ns { dungeonLevel = lv }

setLevelTransition :: NetHackState -> LevelTransition -> NetHackState
setLevelTransition ns transition = ns { levelTransition = Just transition }

resetLevelTransition :: NetHackState -> NetHackState
resetLevelTransition ns = ns { levelTransition = Nothing }

newGame :: RWChan B.ByteString -> NetHackState
newGame channels =
  NetHackState { currentLevelID = 0,
                 levels = M.singleton 0 (fst $ newLevel 0),
                 terminal = T.emptyTerminal 80 24,
                 messages = [],
                 inventory = M.empty,
                 inventoryNeedsUpdate = True,
                 runningID = 1,
                 channels = channels,
                 dungeonLevel = 1,
                 levelTransition = Nothing }

setInventory :: NetHackState -> M.Map Char [Item] -> NetHackState
setInventory ns map = ns { inventory = map }

setInventoryNeedsUpdate :: NetHackState -> Bool -> NetHackState
setInventoryNeedsUpdate ns up = ns { inventoryNeedsUpdate = up }

setTerminal :: NetHackState -> T.Terminal -> NetHackState
setTerminal ns t = ns { terminal = t }

setLevel :: NetHackState -> Level -> NetHackState
setLevel ns level =
  ns { levels = M.insert (levelID level) level oldLevels }
  where
    oldLevels = levels ns

setCurrentLevel :: NetHackState -> Level -> NetHackState
setCurrentLevel ns level =
  ns { currentLevelID = id }
  where
    id = levelID level

update :: NetHackState -> STM NetHackState
update ns@(NetHackState { channels = chan,
                          terminal = oldTerminal }) = do
  newData <- readRWChan chan
  let newTerminal = B.foldl T.handleChar oldTerminal newData
  return $ ns { terminal = newTerminal,
                messages = stripMessages newTerminal }

write :: NetHackState -> B.ByteString -> STM ()
write ns@(NetHackState { channels = chan }) str =
  writeRWChan chan str


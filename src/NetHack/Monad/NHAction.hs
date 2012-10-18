{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module NetHack.Monad.NHAction(get, getTerminal, NHAction(), Feature(..),
                              put, Element(..), Level(..), NetHackState(..),
                              answer, bailout, runNHAction, newGame,
                              getLevel, MonsterInst(..), MonsterAttrs(..),
                              defaultMonsterAttrs,
                              waitForData, liftIO)
                              where

import qualified NetHack.Vanilla.MonsterData as MD
import qualified Data.ByteString.Char8 as B
import Control.Concurrent.STM
import Control.Monad.State
import NetHack.Messages
import NetHack.ReadWriteChan
import NetHack.Alignment
import Data.Array(Array(), array, (!))
import qualified Terminal as T

data Level = Level { number   :: Int,
                     levelId  :: Int,
                     elements :: Array (Int, Int) Element,
                     endGame  :: Bool,
                     boulders :: [(Int, Int)],
                     items    :: [((Int, Int), Item)],
                     monsters :: [((Int, Int), MonsterInst)] }
                     deriving(Show)

data NetHackState = NetHackState { currentLevel :: Level,
                                   terminal :: T.Terminal,
                                   messages :: [String],
                                   runningId :: Int,
                                   channels :: RWChan B.ByteString }

data Element = Element { searched :: Int,
                         walked   :: Int,
                         diggable :: Bool,
                         lookedLike :: (String, T.Attributes),
                         feature  :: [Feature] }
                       deriving(Show)

data Item = Item deriving(Show)
data MonsterAttrs = MonsterAttrs { peaceful :: Maybe Bool,
                                   tame :: Maybe Bool } deriving(Eq, Show)
data MonsterInst = MonsterInst MD.Monster MonsterAttrs
                   deriving(Eq, Show)

defaultMonsterAttrs = MonsterAttrs Nothing Nothing

initialElement :: Element
initialElement = Element { searched = 0,
                           walked = 0,
                           diggable = True,
                           lookedLike = ("karamelli", T.defaultAttrs),
                           feature = [] }

type LevelID = Int

data Feature = DownStairs (Maybe LevelID) |
               UpStairs   (Maybe LevelID) |
               DownLadder (Maybe LevelID) |
               UpLadder   (Maybe LevelID) |
               Portal     (Maybe LevelID) |
               Throne           |
               Floor            |
               Wall             |
               ClosedDoor       |
               OpenedDoor       |
               Grave            |
               Altar (Maybe Alignment) | -- alignment may not be known
               Trap             |  -- TODO: distinguish traps
               Tree             |
               Water            |  -- TODO: distinguish pools and moats
               Lava             |
               DrawbridgeClosed |
               DrawbridgeOpened |
               Cloud            |
               Corridor         |
               Air              |
               Rock             |
               Fountain         |
               IronBars         |
               Sink             |
               Unknown
               deriving(Eq, Show)

newtype NHAction a = NHAction (StateT NetHackState IO a)
                     deriving (MonadState NetHackState, MonadIO, Functor)

instance Monad NHAction where
  return x = NHAction $ return x
  (NHAction m) >>= b  = NHAction $ m >>= (\value -> let x = b value
                                                     in unwrap x)
                        where
                          unwrap (NHAction x) = x

getTerminal :: NHAction T.Terminal
getTerminal = do ns <- get; return $ terminal ns

getLevel :: NHAction Level
getLevel = do ns <- get; return $ currentLevel ns

getChan :: NHAction (RWChan B.ByteString)
getChan = do ns <- get; return $ channels ns

runNHAction :: NetHackState -> NHAction a -> IO a
runNHAction ns (NHAction st) = do
  (result, _) <- runStateT st ns
  return result

newLevel :: Int -> (Level, Int)
newLevel id = (Level { number = 1,
                       levelId = id,
                       elements = array ((1, 2) :: (Int, Int),
                                         (80, 22) :: (Int, Int))
                                        [((x,y), initialElement) |
                                           x <- [1..80],
                                           y <- [2..22]],
                       endGame = False,
                       boulders = [],
                       items = [],
                       monsters = [] },
               id + 1)

newGame :: RWChan B.ByteString -> NetHackState
newGame = NetHackState level (T.emptyTerminal 80 24) [] 1
          where
            (level, _) = newLevel 0

featureStr :: [Feature] -> String
featureStr [Wall] = "#"
featureStr [Floor] = "."
featureStr [OpenedDoor] = "|"
featureStr [] = "%"
featureStr x
  | length x > 1 = "!"
  | otherwise    = "?"

-- For debugging
printOut :: Level -> IO ()
printOut Level { elements = elems } = do
  mapM_ (\x -> putChar '-') [1..82]
  putChar '\n'
  mapM_ (\y -> putChar '|' >>
               (mapM_ (\x -> putStr (featureStr $ feature (elems ! (x, y)))) [1..80]) >>
               putChar '|' >>
               putChar '\n') [2..22]
  mapM_ (\x -> putChar '-') [1..82]
  putChar '\n'

waitForData :: NHAction ()
waitForData = do
  chan <- getChan
  str <- liftIO $ atomically $ readRWChan chan
  ns <- get
  t <- getTerminal
  let t2 = newT str t
  put $ ns { terminal = t2, messages = stripMessages t2 }
  liftIO $ T.printOut t2
  where
    newT str t = B.foldl T.handleChar t str

bailout = error

class Answerable a where
  answer :: a -> NHAction ()

instance Answerable Char where
  answer ch = getChan >>= (\chan ->
    liftIO $ atomically $ writeRWChan chan $ B.pack [ch]) >> waitForData

instance Answerable [Char] where
  answer str = getChan >>= (\chan ->
    liftIO $ atomically $ writeRWChan chan $ B.pack str) >> waitForData


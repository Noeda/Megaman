module NetHack.State(NetHackState(..), BAction(..), NAction(..),
                     Level(..), Element(..), LevelID, Feature(..),
                     NActionReturn(..), newLevel) where

import Data.Array
import NetHack.Alignment
import qualified Terminal as T

data Level = Level { number   :: Int,
                     levelId  :: Int,
                     elements :: Array (Int, Int) Element,
                     endGame  :: Bool,
                     boulders :: [(Int, Int)],
                     items    :: [((Int, Int), Item)],
                     monsters :: [((Int, Int), Monster)] }
                     deriving(Show)

data Element = Element { searched :: Int,
                         walked   :: Int,
                         diggable :: Bool,
                         feature  :: [Feature] }
                       deriving(Show)

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
               Unknown
               deriving(Show)

data Item = Item deriving(Show)
data Monster = Monster deriving(Show)

data NetHackState = NetHackState { currentLevel :: Level,
                                   terminal :: T.Terminal,
                                   messages :: [String],
                                   runningId :: Int,
                                   next :: NAction }

data BAction = BAction NAction NAction
                 (NetHackState -> IO (NetHackState, Bool)) |
               AndAction BAction BAction |
               OrAction BAction BAction |
               NotAction BAction
data NAction = IfAction BAction NAction NAction |
               ModAction (NetHackState -> IO NetHackState) |
               SeqAction NAction NAction |
               StepOutAction NActionReturn |
               RepeatUntilNoAnswer NAction |
               SinkAction |
               BailoutAction String

data NActionReturn = Answer Char |
                     Bailout String


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

initialElement :: Element
initialElement = Element { searched = 0,
                           walked = 0,
                           diggable = True,
                           feature = [] }

module NetHack.Control.Move
  (moveTo,
   tryMoveTo,
   handleTurn,
   moveLetter,
   MoveConfiguration(..))
  where

import NetHack.Monad.NHAction
import NetHack.Control.More
import NetHack.Control.Level
import NetHack.Control.ItemListing
import NetHack.Data.Level

import qualified Terminal.Data as T
import qualified Terminal.Terminal as T

import Control.Monad.IO.Class

handleTurn :: NHAction ()
handleTurn = do
  skipMores
  updateCurrentLevel
  updateInventoryIfNecessary

data MoveConfiguration = IgnoreMonsters |
                         CancelAtMonsters Int |
                         HitMonster

tryMoveTo :: MoveConfiguration -> [Coords] -> NHAction Bool
tryMoveTo _ [] = return False
tryMoveTo conf (c:coords) = do
  cancel <- moveTo conf c
  newCoords <- getCoordsM
  if newCoords == c then return True
                    else if cancel then return False
                                   else tryMoveTo conf coords

moveTo :: MoveConfiguration -> (Int, Int) -> NHAction Bool
moveTo conf target = moveTo2 conf target False
  where
  moveTo2 conf target status = do
    coords <- getCoordsM
    if coords == target
      then return True
      else do l <- getLevelM
              case conf of
                (CancelAtMonsters range) ->
                  do hostiles <- hostileMonstersWithinRangeM range
                     return $ if hostiles /= [] then True else False
                _ -> do liftIO $ putStrLn $ "going to: " ++ show target
                        case findPathTo l coords target of
                         Nothing   -> return False
                         Just path ->
                           do result <- stepTo conf (head path)
                              case result of
                                (Left False) -> return status
                                (Left True) -> moveTo2 conf target True
                                (Right Cancel) -> return status

data Cancel = Cancel

stepTo :: MoveConfiguration -> (Int, Int) -> NHAction (Either Bool Cancel)
stepTo conf target = do
  coords <- getCoordsM
  let letter = moveLetter coords target
  answer letter
  t <- getTerminalM
  cancel <- let hitSomething = T.isSomewhereOnScreen "You kill" t ||
                               T.isSomewhereOnScreen "You destroy" t ||
                               T.isSomewhereOnScreen "You hit" t ||
                               T.isSomewhereOnScreen "You smite" t ||
                               T.isSomewhereOnScreen "You miss" t
             in case conf of
                  HitMonster -> return $ if hitSomething then True
                                                         else False
                  _ -> return False

  updateCurrentLevel
  updateInventoryIfNecessary
  -- Did it actually move where we wanted it to?
  newCoords <- getCoordsM
  case () of _
               | cancel == True -> return (Right Cancel)
               | newCoords == coords ->
                   forbidMovementFromM coords target >> return (Left False)
               | newCoords == target -> return (Left True)
               | otherwise           -> return (Left False)

moveLetter :: (Int, Int) -> (Int, Int) -> Char
moveLetter (x1, y1) (x2, y2)
  | x2 == x1+1 && y2 == y1 = 'l'
  | x2 == x1-1 && y2 == y1 = 'h'
  | x2 == x1 && y2 == y1-1 = 'k'
  | x2 == x1 && y2 == y1+1 = 'j'
  | x2 == x1-1 && y2 == y1-1 = 'y'
  | x2 == x1+1 && y2 == y1-1 = 'u'
  | x2 == x1-1 && y2 == y1+1 = 'b'
  | x2 == x1+1 && y2 == y1+1 = 'n'
  | otherwise =
     error $ "I can't move from " ++ show (x1, y1) ++
             " to " ++ show (x2, y2) ++ " in one step."


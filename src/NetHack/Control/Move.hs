module NetHack.Control.Move
  (moveTo,
   tryMoveTo,
   handleTurn,
   moveLetter)
  where

import NetHack.Monad.NHAction
import NetHack.Control.More
import NetHack.Control.Level
import NetHack.Control.ItemListing
import NetHack.Data.Level

import Control.Monad.IO.Class

handleTurn :: NHAction ()
handleTurn = do
  skipMores
  updateCurrentLevel
  updateInventoryIfNecessary

tryMoveTo :: [Coords] -> NHAction Bool
tryMoveTo [] = return False
tryMoveTo (c:coords) = do
  succeeded <- moveTo c
  newCoords <- getCoordsM
  if newCoords == c || succeeded then return True
                                 else tryMoveTo coords

moveTo :: (Int, Int) -> NHAction Bool
moveTo target = do
  coords <- getCoordsM
  if coords == target
    then return True
    else do l <- getLevelM
            liftIO $ putStrLn $ "going to: " ++ show target
            case findPathTo l coords target of
              Nothing   -> return False
              Just path -> do stepTo (head path)
                              moveTo target

stepTo :: (Int, Int) -> NHAction Bool
stepTo target = do
  coords <- getCoordsM
  let letter = moveLetter coords target
  answer letter
  skipMores
  updateCurrentLevel
  updateInventoryIfNecessary
  -- Did it actually move where we wanted it to?
  newCoords <- getCoordsM
  case () of _
               | newCoords == coords ->
                   forbidMovementFromM coords target >> return False
               | newCoords == target -> return True
               | otherwise           -> return False

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


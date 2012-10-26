module NetHack.AI(root) where

import Control.Monad
import NetHack.Monad.NHAction
import NetHack.Data.Combo
import NetHack.Data.Alignment
import NetHack.Control.ItemListing
import NetHack.Control.Screen
import NetHack.Control.More
import NetHack.Control.Level
import NetHack.Control.Move
import NetHack.Data.Level

import Data.Maybe(fromJust)
import Control.Monad.IO.Class

root :: NHAction ()
root = update >>
       restoreSave >>
       startCharacter (Combo Healer Gnome Female Neutral) >>
       decideAction

-- Restores a save if it looks like there is one.
-- (currently saving the state is not implemented)
restoreSave :: NHAction ()
restoreSave =
  void $ ifIn restoringSave (answer ' ')

-- Starts a character if one has not been started yet.
-- If the combo is invalid or something else bad happens, it bails out.
startCharacter :: Combo -> NHAction ()
startCharacter combo = do
  ifIn shallIPick       (answer 'n')
  ifIn pickARole        (answer role1)
  ifIn pickTheRace      (answer race1)
  ifIn pickTheGender    (answer gender1)
  ifIn pickTheAlignment (answer alignment1)

  skipMores

  void $ ifNotIn gameScreen (bailout "I was not able to pick a character.")

  where
    role1 = roleLetter $ role combo
    race1 = raceLetter $ race combo
    gender1 = genderLetter $ gender combo
    alignment1 = alignmentLetter $ alignment combo

-- What to do...what to do
decideAction :: NHAction ()
decideAction = do
  -- Explore!
  exploreLevel
  -- Kill monsters!
  killMonsters
  -- Search walls!
  void searchWalls

findAndOpenDoors :: NHAction Bool
findAndOpenDoors = do
  handleTurn
  l <- getLevelM
  coords <- getCoordsM
  let closedDoors = concatMap neighbourCoordinates $ findClosedDoors l
  if closedDoors == []
    then return False
    else do succeeded <- tryMoveTo (sortByDistance coords closedDoors)
            if succeeded then openNeighbourDoors
                         else return False

openNeighbourDoors :: NHAction Bool
openNeighbourDoors = openNeighbourDoors2 False
  where
  openNeighbourDoors2 :: Bool -> NHAction Bool
  openNeighbourDoors2 status = do
    handleTurn
    l <- getLevelM
    coords <- getCoordsM
    let closedDoors = filter (isNextTo coords) $ findClosedDoors l
    openNeighbourDoors3 l coords closedDoors status

  openNeighbourDoors3 :: Level -> Coords -> [Coords] -> Bool -> NHAction Bool
  openNeighbourDoors3 _ _ [] status = return status
  openNeighbourDoors3 l coords closedDoors status = do
    answer $ "o" ++ [direction]
    yay <- liftM2 (||) (isSomewhereOnScreen "This door is already open.")
                       (isSomewhereOnScreen "You see no door there.")
    if yay then maybeMarkAsOpenDoorM closedDoor >> return True
           else do
    locked <- isSomewhereOnScreen "This door is locked."
    if locked
      then do answer $ control 'D' ++ [direction]
              openNeighbourDoors2 True
      else do resists <- isSomewhereOnScreen "The door resists!"
              if resists then openNeighbourDoors2 True
                         else handleTurn >> return True

    where
      direction = moveLetter coords closedDoor
      closedDoor = head closedDoors

-- Explores the dungeon level as specified by an integer.
-- If the player is not the level, then the bot will attempt to get there
-- in some way. May bail out if it runs out of ideas to get on the level.
exploreLevel :: NHAction Bool
exploreLevel = exploreLevel2 False
  where
  exploreLevel2 status = do
    handleTurn
    l <- getLevelM
    coords <- getCoordsM
    -- Find interesting places to explore.
    let places = explorableReachablePositions l coords
    liftIO $ putStrLn $ show (sortByDistance coords places)
    if places == []
      then doorFinding
      else do succeeded <- tryMoveTo (sortByDistance coords places)
              if succeeded then exploreLevel2 True
                           else doorFinding

    where
      doorFinding = do openedDoors <- findAndOpenDoors
                       if openedDoors then exploreLevel2 status
                                      else return status


killMonsters :: NHAction Bool
killMonsters = return False

searchWalls :: NHAction Bool
searchWalls = return False


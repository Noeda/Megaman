module NetHack.Control.ItemListing
  (updateInventoryIfNecessary,
   itemsOnScreen)
  where

import Control.Monad
import Control.Monad.IO.Class(liftIO)
import Data.Maybe(fromJust)
import NetHack.Data.NetHackState
import NetHack.Monad.NHAction
import NetHack.Data.Item
import qualified Data.Map as M
import qualified Terminal.Data as T
import qualified Terminal.Terminal as T
import qualified Regex as R
import NetHack.Control.Screen(isSomewhereOnScreen)

resetInventoryNeedsUpdate :: NHAction Bool
resetInventoryNeedsUpdate = do
  ns <- get
  let up = inventoryNeedsUpdate ns
  putInventoryNeedsUpdateM False
  return up

updateInventoryIfNecessary :: NHAction ()
updateInventoryIfNecessary = do
  needsUpdate <- resetInventoryNeedsUpdate
  when needsUpdate $ do
    answer 'i'
    emptyInventory <- isSomewhereOnScreen "Not carrying anything."
    if emptyInventory
      then putInventoryM M.empty
      else updateInventoryFromScreen

itemsOnScreen :: (Item -> Bool) -> NHAction (M.Map Char Item)
itemsOnScreen select = do
  t <- getTerminalM
  readOutItems select $ detectItemListingPosition t

updateInventoryFromScreen :: NHAction ()
updateInventoryFromScreen = do
  t <- getTerminalM
  items <- readOutItems never $ detectItemListingPosition t
  putInventoryM items
  where
    never _ = False

readOutItems :: (Item -> Bool) -> (Int, Int, Int) -> NHAction (M.Map Char Item)
readOutItems select (xStart, yStart, yEnd) = do
    t <- getTerminalM
    readOutItem t yStart M.empty
  where
  readOutItem t y accum
    | y > yEnd                              = return accum
    | fromJust $ R.match "^\\((end)\\) " line = do answer ' '
                                                   return accum
    | fromJust $ R.match "^\\(([0-9]+) of [0-9]+\\) " line =
        do answer ' '
           let continues = concat
                           [T.isSomewhereOnScreenPosWithAttributes
                              "(end) " T.defaultAttributes t,
                            T.isSomewhereOnScreenPosWithAttributesRegex
                              "^\\(([0-9]+) of [0-9]+\\) "
                              T.defaultAttributes t]
           if length continues > 0 then readOutItem t yStart accum
                                   else return $ accum
    | fromJust $ R.match regex line =
        let letter = head line
            itemname = fromJust $ (R.match regex line :: Maybe String)
         in do let item = canonicalizeItemName itemname
               let items = canonicalizeItemToInventory accum letter item
               when (select item) $ answer letter
               readOutItem t (y+1) items
    | otherwise = readOutItem t (y+1) accum
    where
      regex = "^[a-zA-Z] \\- (.+)$"
      line = drop (xStart - 1) $ T.lineAt y t

canonicalizeItemToInventory :: M.Map Char Item -> Char -> Item ->
                               M.Map Char Item
canonicalizeItemToInventory map ch item = M.insert ch item map

detectItemListingPosition :: T.Terminal -> (Int, Int, Int)
detectItemListingPosition t =
  -- We try the most 'reliable' methods first, then trying less reliable
  -- ones. For actual inventory screen (from using the 'i' key) there are
  -- captions with "Weapons", "Armor" and so on. However, it will end in
  -- either "(end) X" or "(x of y)X" where X is the position of the cursor.
  case testW "(end) " of
    [(x, y)] -> if T.coords t == (x+6,y)
                  then (x, 1, y)
                  else detectItemListingPosition2
    _        -> detectItemListingPosition2

  where
  testW =
    (\x -> T.isSomewhereOnScreenPosWithAttributes x T.defaultAttributes t)

  detectItemListingPosition2 =

    -- This one appears in 'i' when there is more than one page of items.
    case T.isSomewhereOnScreenPosWithAttributesRegex "^\\(([0-9]+) of [0-9]+\\) "
            T.defaultAttributes t of
      [(x, y)] -> if snd (T.coords t) == y
                    then (x, 1, y)
                    else error $ "Cursor is not where I'd expect it to be in "
                              ++ "item listing."
      _        -> error "I could not detect an item listing."

  {- Disabled for now; the item listing is different when this happens.
   - We'll use different ways to read items.
  detectItemListingPosition3 =
    -- This appears when : is pressed on top of an item pile.
    case testW "Things that are here:" of
      [(x, y)] -> (x, 1, snd (T.coords t) - 1)
      _        -> error "I could not detect an item listing." -}


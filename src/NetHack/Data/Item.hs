module NetHack.Data.Item
  (Item(),
   unknownItem,
   couldHaveItems)
  where

import NetHack.Data.Appearance

data Item = UnknownItem String deriving(Show, Eq)

unknownItem :: String -> Item
unknownItem = UnknownItem

class CouldHaveItemsable a where
  couldHaveItems :: a -> Bool

instance CouldHaveItemsable Appearance where
  couldHaveItems (str, _) = couldHaveItems str

instance CouldHaveItemsable String where
  couldHaveItems = isItemString
    where
      isItemString "?"  = True
      isItemString "!"  = True
      isItemString "\"" = True
      isItemString "("  = True
      isItemString ")"  = True
      isItemString "["  = True
      isItemString "%"  = True
      isItemString "="  = True
      isItemString "/"  = True
      isItemString "*"  = True
      isItemString "+"  = True
      isItemString _    = False


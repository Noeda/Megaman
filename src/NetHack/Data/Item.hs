module NetHack.Data.Item
  (Item(),
   couldHaveItems)
  where

import NetHack.Data.Appearance

data Item = Item deriving(Show, Eq)

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


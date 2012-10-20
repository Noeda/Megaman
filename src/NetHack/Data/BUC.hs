module NetHack.Data.BUC
  (BUC(..))
  where

data BUC = Blessed | Uncursed | Cursed
           deriving(Eq, Show)


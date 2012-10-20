module NetHack.Data.Item
  (Item(),
   item,
   quantity,
   buc,
   enchantment,
   charging,
   couldHaveItems,
   canonicalizeItemName)
  where

import Data.List(isPrefixOf)
import NetHack.Data.Appearance
import NetHack.Data.BUC
import NetHack.Data.Messages(trim)

import Regex(match)

data Item = Item ItemStatus String deriving(Show, Eq)

data ItemStatus = ItemStatus { quantity :: Maybe Int,
                               buc :: Maybe BUC,
                               enchantment :: Maybe Int,
                               charging :: Maybe (Int, Int) }
                             deriving(Show, Eq)

item :: ItemStatus -> String -> Item
item = Item

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

canonicalizeItemName :: String -> Item
canonicalizeItemName str =
  let (status, canon) = canonicalize str2
   in Item status (removePlural canon)
  where
    str2 = (removeIndefiniteArticles . trim $ str)

canonicalize :: String -> (ItemStatus, String)
canonicalize str =
  case match "^([0-9]+ )(.+)" str :: Maybe (String, String) of
    Nothing               -> canonicalize2 Nothing str
    Just (quantity, rest) -> canonicalize2 (Just quantity) rest

  where
  canonicalize2 quantity str =
    case (match "^(blessed|uncursed|cursed) (.+)" str) :: [String] of
      [] -> canonicalize3 quantity Nothing str
      [buc,rest] -> canonicalize3 quantity (Just buc) rest
  canonicalize3 quantity buc str =
    case (match ("^(corroded|very corroded|thoroughly corroded|" ++
               "rusty|very rusty|thoroughly rusty|" ++
               "rotted|very rotted|thoroughly rotted|" ++
               "burnt|very burnt|thoroughly burnt|" ++
               "fixed|rotproof|fireproof|rustproof) (.+)") str) :: [String]
               of
      [] -> canonicalize4 quantity buc str
      [_,rest] -> canonicalize4 quantity buc rest
  canonicalize4 quantity buc str =
    case (match "^((\\+|-)[0-9]+ )(.+)" str) :: [String] of
      []                        -> canonicalize5 quantity buc Nothing str
      [enchantment, _, rest] -> canonicalize5 quantity buc
                                          (Just enchantment) rest

  canonicalize5 quantity buc enchantment str =
    case (match "^(.+) \\(((\\+|-)?([0-9]+)):(((\\+|-)?[0-9]+))\\)" str) :: [String] of
      [] -> canonicalize6 quantity buc enchantment Nothing str
      [rest, charged, _, _, charges, _, _] ->
        canonicalize6 quantity buc enchantment (Just (charged, charges)) rest

  canonicalize6 quantity buc enchantment charging str =
    (ItemStatus { quantity = fmap convertQuantity quantity,
                  enchantment = fmap convertEnchantment enchantment,
                  buc = fmap convertBUC buc,
                  charging = fmap convertCharging charging },
      case match ("^(.+) (\\(weapon in .*\\)|" ++
                 "\\(alternate weapon; not wielded\\)|" ++
                 "\\(being worn\\)|" ++
                 "\\(in quiver\\)|" ++
                 "\\(lit\\)|" ++
                 "\\(embedded in .*\\))") str :: [String] of
        [] -> str
        (rest:_) -> rest)

removeIndefiniteArticles :: String -> String
removeIndefiniteArticles ('a':' ':rest) = rest
removeIndefiniteArticles ('a':'n':' ':rest) = rest
removeIndefiniteArticles rest = rest

convertInteger :: String -> Int
convertInteger ('+':rest) = convertInteger rest
convertInteger x = read x :: Int

convertQuantity = convertInteger
convertEnchantment = convertInteger

convertBUC :: String -> BUC
convertBUC "blessed" = Blessed
convertBUC "cursed" = Cursed
convertBUC "uncursed" = Uncursed

convertCharging :: (String, String) -> (Int, Int)
convertCharging (s1, s2) = (convertInteger s1, convertInteger s2)

removePlural :: String -> String
removePlural str = removeGeneric . removePotions . removeScrolls .
                   removeMothersInLaw . removeFathersInLaw .
                   removeKnives $ str
  where
    removeKnives = replaceInfix "knives" "knife"
    removeScrolls = replaceInfix "scrolls" "scroll"
    removePotions = replaceInfix "potions" "potion"
    removeMothersInLaw = replaceInfix "mothers-in-law" "mother-in-law"
    removeFathersInLaw = replaceInfix "fathers-in-law" "father-in-law"
    removeGeneric str = if last str == 's' then take (length str - 1) str
                                           else str

replaceInfix :: String -> String -> String -> String
replaceInfix = replaceInfix2 []
  where
  replaceInfix2 accum _ _ [] = reverse accum
  replaceInfix2 accum template replace str@(x:rest) =
    if template `isPrefixOf` str
      then replaceInfix2 (reverse replace ++ accum) template replace
                         (drop (length template) str)
      else replaceInfix2 (x:accum) template replace rest


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
                               charging :: Maybe (Int, Int),
                               greased :: Bool,
                               naming :: Maybe String }
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
      isItemString "$"  = True
      isItemString "`"  = True
      isItemString "´"  = True
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

-- TODO: refactor
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
    case match "^greased (.+)" str :: [String] of
      [] -> canonicalize4 quantity buc False str
      [rest] -> canonicalize4 quantity buc True rest

  canonicalize4 quantity buc greased str =
    case (match ("^(corroded|very corroded|thoroughly corroded|" ++
               "rusty|very rusty|thoroughly rusty|" ++
               "rotted|very rotted|thoroughly rotted|" ++
               "burnt|very burnt|thoroughly burnt|" ++
               "fixed|rotproof|fireproof|rustproof) (.+)") str) :: [String]
               of
      [] -> canonicalize5 quantity buc greased str
      [_,rest] -> canonicalize5 quantity buc greased rest

  canonicalize5 quantity buc greased str =
    case (match "^((\\+|-)[0-9]+ )(.+)" str) :: [String] of
      []                        -> canonicalize6 quantity buc greased
                                          Nothing str
      [enchantment, _, rest] -> canonicalize6 quantity buc greased
                                          (Just enchantment) rest

  canonicalize6 quantity buc greased enchantment str =
    case (match "^(.+) \\(((\\+|-)?([0-9]+)):(((\\+|-)?[0-9]+))\\)" str) :: [String] of
      [] -> canonicalize7 quantity buc greased enchantment Nothing str
      [rest, charged, _, _, charges, _, _] ->
        canonicalize7 quantity buc greased enchantment
           (Just (charged, charges)) rest

  canonicalize7 quantity buc greased enchantment charging str =
    case match ("^(.+) (\\(weapon in .*\\)|" ++
               "\\(alternate weapon; not wielded\\)|" ++
               "\\(being worn\\)|" ++
               "\\(in quiver\\)|" ++
               "\\(lit\\)|" ++
               "\\(embedded in .*\\))") str :: [String] of
      [] -> canonicalize8 quantity buc greased enchantment charging str
      (rest:_) -> canonicalize8 quantity buc greased enchantment charging rest

  canonicalize8 quantity buc greased enchantment charging str =
    case match "^(.+) named (.+)$" str :: [String] of
      [] -> canonicalize9 quantity buc greased enchantment charging Nothing str
      [rest,naming] ->
        canonicalize9 quantity buc greased enchantment charging
           (Just naming) rest

  canonicalize9 quantity buc greased enchantment charging naming str =
    (ItemStatus { quantity = fmap convertQuantity quantity,
                  enchantment = fmap convertEnchantment enchantment,
                  buc = fmap convertBUC buc,
                  greased = greased,
                  charging = fmap convertCharging charging,
                  naming = naming },
      str)

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
                   removePieces .
                   removeKnives $ str
  where
    removeKnives = replaceInfix "knives" "knife"
    removeScrolls = replaceInfix "scrolls" "scroll"
    removePotions = replaceInfix "potions" "potion"
    removePieces = replaceInfix "pieces" "piece"
    removeMothersInLaw = replaceInfix "mothers-in-law" "mother-in-law"
    removeFathersInLaw = replaceInfix "fathers-in-law" "father-in-law"
    removeGeneric str = if last str == 's' &&
                           last (take (length str - 1) str) /= 's'
                         then take (length str - 1) str
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


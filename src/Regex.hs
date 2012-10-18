{-# LANGUAGE FlexibleInstances #-}

module Regex(Matcher, RMatch, match) where

import qualified Text.Regex.TDFA.String as R
import qualified Text.Regex.Base.RegexLike as RL

import Misc
import Safe
import Data.Array((!), bounds)

type RMatch a = String -> String -> Maybe a

class Matcher a where
  match     :: String -> String -> Maybe a

instance Matcher [Char] where
  match = compileAndExecute

instance Matcher Int where
  match regex str = case compileAndExecute regex str of
                      Nothing -> Nothing
                      Just str -> readMay str :: Maybe Int


compile :: String -> R.Regex
compile str =
  case R.compile RL.defaultCompOpt RL.defaultExecOpt str of
    (Left msg) -> error $ "Regex compilation error: " ++ msg -- I want to know
                                                             -- if our regexes
                                                             -- are wrong
    (Right regex) -> regex
  where
    errorNoSubmatch = error $
      "Expected regex to return at least one submatch. " ++
      "(offending regex: " ++ str ++ ")"

compileAndExecute regex str =
  case R.execute (compile regex) str of
    Left msg      -> error $ "Regex execution error: " ++ msg
    Right Nothing -> Nothing
    Right (Just matches) ->
     if len < 1
       then errorNoSubmatch
       else Just $
            take (snd (matches ! 1))
              (drop
               (fst (matches ! 1))
               str)
      where
        len = snd (bounds matches)
        errorNoSubmatch = error $
          "Expected regex to return at least one submatch. " ++
          "(offending regex: " ++ str ++ ")"


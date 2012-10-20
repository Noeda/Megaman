{-# LANGUAGE FlexibleInstances #-}

module Regex(Matcher, match) where

import qualified Text.Regex.TDFA.String as R
import qualified Text.Regex.Base.RegexLike as RL

import Misc
import Safe
import Data.Array((!), bounds)

class Matcher a where
  match     :: String -> String -> a

instance Matcher (Maybe [Char]) where
  match = compileAndExecute1

instance Matcher (Maybe ([Char], [Char])) where
  match regex str = case (match regex str) :: [String] of
                      []         -> Nothing
                      (x:y:_) -> Just (x, y)

instance Matcher [[Char]] where
  match = compileAndExecuteList

instance Matcher (Maybe Int) where
  match regex str = case compileAndExecute1 regex str of
                      Nothing -> Nothing
                      Just str -> readMay str :: Maybe Int

instance Matcher (Maybe Bool) where
  match regex str = case compileAndExecuteList regex str of
                      [] -> Just False
                      _  -> Just True

compile :: String -> R.Regex
compile str =
  case R.compile RL.defaultCompOpt RL.defaultExecOpt str of
    (Left msg) -> error $ "Regex compilation error: " ++ msg -- I want to know
                                                             -- if our regexes
                                                             -- are wrong
    (Right regex) -> regex

compileAndExecute regex str =
  case R.execute (compile regex) str of
    Left msg      -> error $ "Regex execution error: " ++ msg
    Right Nothing -> Nothing
    Right (Just matches) -> Just matches

compileAndExecute1 regex str =
  case compileAndExecute regex str of
    Nothing -> Nothing
    Just matches ->
     if matchesLen matches < 1
       then errorNoSubmatch
       else Just $ extractSubmatch matches str 1
     where
       errorNoSubmatch = error $
         "Expected regex to return at least one submatch. " ++
         "(offending regex: " ++ str ++ ")"

compileAndExecuteList :: String -> String -> [String]
compileAndExecuteList regex str =
  case compileAndExecute regex str of
    Nothing -> []
    Just matches -> reverse $
     foldl (\results match -> extractSubmatch matches str match:results)
          [] [1..matchesLen matches]

matchesLen = snd . bounds

extractSubmatch matches str n =
   take (snd (matches ! n))
     (drop
      (fst (matches ! n))
      str)


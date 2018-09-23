module OddQuestion where

import Data.Strings (strToLower)

import Lib.QuickIO

oddQuestion :: IO ()
oddQuestion = do
  puts "Say what?"
  youSaid <- gets
  puts $ (quote youSaid) ++ ", huh?"
  response <- gets
  if is_confirmation response
  then puts "Damn right!"
  else puts "Yeah, fuck that."


quote :: String -> String
quote s = [q] ++ s ++ [q]
  where q = '"'

is_confirmation :: String -> Bool
is_confirmation s = (strToLower s) `elem` confirmation_strings
  where confirmation_strings = ["yes", "yep"]
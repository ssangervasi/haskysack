module Main where

import Data.Strings (strToLower)
import qualified Lib.RFC2550

gets = getLine
puts = putStrLn

main :: IO ()
main = partitioner

partitioner :: IO ()
partitioner = do
  puts "Initial partition size:"
  initSize <- gets
  let partitioner = [1, 2, 3]
  let putParts p = puts $ "Part: " ++ (show p)
  map putParts partitioner
  return ()

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
module Lib.QuickIO where

import Text.Read (readMaybe)

gets = getLine
puts = putStrLn

geti :: IO Int
geti = do
  s <- gets
  maybe
    -- Recurse on invalid input
    geti
    -- Wrap result in minimal IO
    return
    -- Try to parse
    (readMaybe s :: Maybe Int)

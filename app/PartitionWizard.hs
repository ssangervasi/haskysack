module PartitionWizard (
  interactiveWizard
) where

import Control.Monad (when)
import Text.Read (readMaybe)

interactiveWizard :: IO ()
interactiveWizard = do
  puts "Initial partition size:"
  initSize <- geti
  sequence $ map putParts $ partitioner initSize
  interactiveWizard

partitioner :: Int -> [Int]
partitioner size = [1..size]

putParts :: Show a => a -> IO ()
putParts p = puts $ "Part: " ++ (show p)


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

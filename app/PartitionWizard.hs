module PartitionWizard (
  interactiveWizard
) where

import Control.Monad (when)

import Lib.QuickIO
import Lib.Partitioner

interactiveWizard :: IO ()
interactiveWizard = do
  puts "Initial partition size:"
  initSize <- geti
  let partition = Partition "Init" initSize
  puts $ label partition
  -- puts $ show $ size partition
  puts . show . size $ partition
  putParts $ enumerate initSize
  interactiveWizard

enumerate :: Int -> [Int]
enumerate size = [1..size]

putParts :: Show a => [a] -> IO ()
putParts ps = do
  sequence $ map putPart ps
  return ()

putPart :: Show a => a -> IO ()
putPart p = puts $ "Part: " ++ (show p)


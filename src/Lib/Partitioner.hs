module Lib.Partitioner where

data Partition =
  Partition {
    label :: Label,
    size :: Size
  }

type Label = String 
type Size = Int

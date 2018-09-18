module Conway (
  gameOfLife
) where

import qualified Data.Matrix as M

gameOfLife :: (Integral a) => a -> M.Matrix Bool
gameOfLife width = M.matrix intWidth intWidth (\_ -> False)
  where intWidth = fromIntegral width

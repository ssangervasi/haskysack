module Main where

import Conway

main :: IO ()
main = do
  putStrLn $ show $ Conway.gameOfLife 10


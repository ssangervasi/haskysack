module Lib (
  someFunc,
  popAround,
  Horse(..)
) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

popAround :: [a] -> [a]
popAround [] = []
popAround (first:rest) = rest ++ [first]

data Horse = Horse {
  name :: String,
  hooves :: Int 
}

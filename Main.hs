module Main where

import qualified Zeiterfassung (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Zeiterfassung.someFunc

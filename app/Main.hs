module Main where

import           Lib
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  countLines $ args !! 1

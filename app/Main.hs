module Main where

import           Lib
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let dir = args !! 1
  lines <- countLines dir
  commits <- countCommits dir
  printLines commits lines
  putStrLn "\n"

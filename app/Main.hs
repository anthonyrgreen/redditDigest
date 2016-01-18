module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  args <- getArgs
  let subreddit = head args
  let filename = "/Users/anthony/hot"
  downloadSubredditToHtml subreddit $ filename ++ ".html"

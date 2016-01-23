module Main where

import System.Environment
import Lib

main :: IO ()
main = do
  [subreddit, title] <- getArgs
  let filename = "/Users/anthony/hot.html"
  downloadSubredditToHtml subreddit filename title

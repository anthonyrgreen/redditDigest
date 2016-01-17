module Main where

import Lib

main :: IO ()
main = do
  let subreddit = "askreddit"
  let filename = "/Users/anthony/hot"
  downloadSubredditToJson subreddit $ filename ++ ".json"
  downloadSubredditToHtml subreddit $ filename ++ ".html"
